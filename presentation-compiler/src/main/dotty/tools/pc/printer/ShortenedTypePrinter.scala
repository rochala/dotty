package dotty.tools.pc.printer

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.NameOrdering
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Variances.varianceSign
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.dotc.printing.*
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.AutoImports.ImportSel
import dotty.tools.pc.AutoImports.ImportSel.Direct
import dotty.tools.pc.AutoImports.ImportSel.Rename
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.IndexedContext.Result
import dotty.tools.pc.Params
import dotty.tools.pc.SemanticdbSymbols
import dotty.tools.pc.utils.MtagsEnrichments.*
import org.eclipse.lsp4j.TextEdit

import scala.collection.mutable
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.dotc.util.Signatures.Param

/**
 * A type printer that shortens types by replacing fully qualified names with shortened versions.
 *
 * The printer supports symbol renames found in scope and will use the rename if it is available.
 * It also handle custom renames as specified in the `renameConfigMap` parameter.
 */
class ShortenedTypePrinter(
    symbolSearch: SymbolSearch,
    includeDefaultParam: ShortenedTypePrinter.IncludeDefaultParam = IncludeDefaultParam.ResolveLater,
    isTextEdit: Boolean = false,
    renameConfigMap: Map[Symbol, String] = Map.empty
)(using indexedCtx: IndexedContext, reportCtx: ReportContext) extends RefinedPrinter(indexedCtx.ctx):
  private val missingImports: mutable.Set[ImportSel] = mutable.LinkedHashSet.empty
  private val defaultWidth = 1000


  def flagsFilter(sym: Symbol) =
    if sym.isType then sym.flags & TypeSourceModifierFlags &~ JavaStatic
    else if sym.isPatternBound then sym.flags & TermSourceModifierFlags &~ Case &~ JavaStatic
    else sym.flags & TermSourceModifierFlags &~ JavaStatic

  private val foundRenames = collection.mutable.LinkedHashMap.empty[Symbol, String]

  def getUsedRenames: Map[Symbol, String] = foundRenames.toMap

  def getUsedRenamesInfo(using Context): List[String] =
    foundRenames.map { (from, to) => s"type $to = ${from.showName}"
    }.toList

  def expressionType(tpw: Type)(using Context): Option[String] =
    tpw match
      case t: PolyType =>
        expressionType(t.resType)
      case t: MethodType =>
        expressionType(t.resType)
      case i: ImportType =>
        expressionType(i.expr.typeOpt)
      case c: ConstantType =>
        Some(tpe(c.underlying))
      case _ if !tpw.isErroneous =>
        Some(tpe(tpw))
      case _ => None

  /**
   * Returns a list of TextEdits (auto-imports) of the symbols
   */
  def imports(autoImportsGen: AutoImportsGenerator): List[TextEdit] =
    missingImports
      .toList
      .filterNot(selector => selector.sym.isRoot)
      .sortBy(_.sym.effectiveName)
      .flatMap(selector => autoImportsGen.renderImports(List(selector)))

  sealed trait SymbolRenameSearchResult:
    val owner: Symbol
    val rename: String
    val prefixAfterRename: List[Symbol]

    def toPrefixText: Text =
      Str(rename) ~ prefixAfterRename.foldLeft(Text())((acc, sym) =>
        acc ~ "." ~ toText(sym.name)
      ) ~ "."

  case class Found(owner: Symbol, rename: String, prefixAfterRename: List[Symbol]) extends SymbolRenameSearchResult
  case class Missing(owner: Symbol, rename: String, prefixAfterRename: List[Symbol]) extends SymbolRenameSearchResult

  /**
   *  In shortened type printer, we don't want to omit the prefix unless it is empty package
   *  All the logic for prefix omitting is implemented in `toTextPrefixOf`
   */
  override protected def isOmittablePrefix(sym: Symbol): Boolean =
    isEmptyPrefix(sym)

  private def findPrefixRename(
      prefix: Symbol
  ): Option[SymbolRenameSearchResult] =
    def ownersAfterRename(owner: Symbol): List[Symbol] =
      prefix.ownersIterator.takeWhile(_ != owner).toList

    val prefixIterator = if isTextEdit then prefix.ownersIterator else Iterator(prefix)

    prefixIterator.flatMap { owner =>
      val prefixAfterRename = ownersAfterRename(owner)
      val ownerRename = indexedCtx.rename(owner)
        ownerRename.foreach(rename => foundRenames += owner -> rename)
      val currentRenamesSearchResult =
        ownerRename.map(Found(owner, _, prefixAfterRename))
      lazy val configRenamesSearchResult =
        renameConfigMap.get(owner).map(Missing(owner, _, prefixAfterRename))
      currentRenamesSearchResult orElse configRenamesSearchResult
    }.nextOption

  private def isAccessibleStatically(sym: Symbol): Boolean =
    sym.isStatic || // Java static
      sym.maybeOwner.ownersIterator.forall { s =>
        s.is(Package) || s.is(Module)
      }

  private def optionalRootPrefix(sym: Symbol): Text =
    // If the symbol has toplevel clash we need to prepend `_root_.` to the symbol to disambiguate
    // it from the local symbol. It is only required when we are computing text for text edit.
    if isTextEdit && indexedCtx.toplevelClashes(sym) then
      Str("_root_.")
    else
      Text()

  private def findRename(tp: NamedType): Option[Text] =
    val maybePrefixRename = findPrefixRename(tp.symbol.maybeOwner)

    if maybePrefixRename.exists(importRename => indexedCtx.findSymbol(importRename.rename).isDefined) then
      Some(super.toTextPrefixOf(tp))
    else
      maybePrefixRename.map {
        case res: Found => res.toPrefixText
        case res: Missing =>
          val importSel =
            if res.owner.name.toString == res.rename then
              ImportSel.Direct(res.owner)
            else ImportSel.Rename(res.owner, res.rename)

          missingImports += importSel
          res.toPrefixText
      }


  override def toTextPrefixOf(tp: NamedType): Text = controlled {
    val maybeRenamedPrefix: Option[Text] = findRename(tp)
    val trimmedPrefix: Text =
      if !tp.designator.isInstanceOf[Symbol] && tp.typeSymbol == NoSymbol then
        maybeRenamedPrefix.getOrElse(super.toTextPrefixOf(tp))
      else
        indexedCtx.lookupSym(tp.symbol) match
          // symbol is missing and is accessible statically, we can import it and add proper prefix
          case Result.Missing if isAccessibleStatically(tp.symbol) =>
            maybeRenamedPrefix.getOrElse:
              missingImports += ImportSel.Direct(tp.symbol)
              Text()
          // the symbol is in scope, we can omit the prefix
          case Result.InScope => Text()
          // the symbol is in conflict, we have to include prefix to avoid ambiguity
          case Result.Conflict =>
            maybeRenamedPrefix.getOrElse(super.toTextPrefixOf(tp))
          case _ => super.toTextPrefixOf(tp)

    optionalRootPrefix(tp.symbol) ~ trimmedPrefix
  }

  override protected def selectionString(tp: NamedType): String =
    indexedCtx.rename(tp.symbol) match
      case Some(value) =>
        foundRenames += tp.symbol -> value
        value
      case None => super.selectionString(tp)

  override def toText(tp: Type): Text =
    tp match
      case c: ConstantType => toText(c.value)
      case tp: MethodType if tp.paramNames.nonEmpty && tp.paramNames.forall(_.toString.startsWith(ContextBoundParamName.separator)) =>
        (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly])
        ~ super.toText(tp.resultType)

      case tp: MethodType =>
        changePrec(GlobalPrec) {
          "("
          ~ keywordText("using ").provided(tp.isContextualMethod)
          ~ keywordText("implicit ").provided(tp.isImplicitMethod && !tp.isContextualMethod)
          ~ paramsText(tp)
          ~ ")"
          ~ (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly])
          ~ toText(tp.resultType)
        }
      case tp if tp.isError => super.toText(indexedCtx.ctx.definitions.AnyType)
      case _ => super.toText(tp)

  override def toTextSingleton(tp: SingletonType): Text =
    tp match
      case ConstantType(const) => toText(const)
      case _ => toTextRef(tp) ~ ".type"

  def tpe(tpe: Type): String = toText(tpe).mkString(defaultWidth, false)

  def hoverSymbol(sym: Symbol, info: Type)(using Context): String =
    val typeSymbol = info.typeSymbol

    def shortTypeString: String = tpe(info)

    def ownerTypeString: String =
      typeSymbol.owner.fullNameBackticked

    def name: String = nameString(sym)

    sym match
      case p if p.is(Flags.Package) =>
        s"package ${p.fullNameBackticked}"
      case c if c.is(Flags.EnumVal) =>
        s"case $name: $shortTypeString"
      // enum
      case e if e.is(Flags.Enum) || sym.companionClass.is(Flags.Enum) =>
        s"enum $name: $ownerTypeString"
      /* Type cannot be shown on the right since it is already a type
       * let's instead use that space to show the full path.
       */
      case o if typeSymbol.is(Flags.Module) => // enum
        s"${keyString(o)} $name: $ownerTypeString"
      case m if m.is(Flags.Method) =>
        dclText(m.mapInfo(_ => info)).mkString(defaultWidth, false)
      case _ =>
        val implicitKeyword =
          if sym.is(Flags.Implicit) then List("implicit") else Nil
        val finalKeyword = if sym.is(Flags.Final) then List("final") else Nil
        val keyOrEmpty = keyString(sym)
        val keyword =
          if keyOrEmpty.iterator.nonEmpty then List(keyOrEmpty) else Nil
        (implicitKeyword ::: finalKeyword ::: keyword ::: (s"$name:" :: shortTypeString :: Nil))
          .mkString(" ")
    end match
  end hoverSymbol

  def isImportedByDefault(sym: Symbol): Boolean =
    import dotty.tools.dotc.core.Symbols.defn
    lazy val effectiveOwner = sym.effectiveOwner
    sym.isType && (effectiveOwner == defn.ScalaPackageClass || effectiveOwner == defn.ScalaPredefModuleClass)

  def completionSymbol(denotation: Denotation): String =
    val info = denotation.info.widenTermRefExpr
    val typeSymbol = info.typeSymbol
    val sym = denotation.symbol

    lazy val typeEffectiveOwner =
      if typeSymbol != NoSymbol then " " + fullNameString(typeSymbol.effectiveOwner)
      else " " + fullNameString(sym.effectiveOwner)

    if isImportedByDefault(sym) then typeEffectiveOwner
    else if sym.is(Flags.Package) || sym.isClass then " " + fullNameString(sym.effectiveOwner)
    else if sym.is(Flags.Module) || typeSymbol.is(Flags.Module) then typeEffectiveOwner
    else if sym.is(Flags.Method) then toRichText(denotation.asSingleDenotation).mkString(defaultWidth, false)
    else if sym.isType then
      info match
        case TypeAlias(t) => " = " + tpe(t.resultType)
        case t => tpe(t.resultType)
    else tpe(info)
    end if
  end completionSymbol

  override protected def toTextRHS(tp: Type, isParameter: Boolean = false): Text = controlled {
    tp match
      case tp: MethodType if tp.paramNames.nonEmpty && tp.paramNames.forall(_.toString.startsWith(ContextBoundParamName.separator)) =>
        (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly])
        ~ super.toText(tp.resultType)
      case tp: PolyType => toText(tp)
      case _ => super.toTextRHS(tp, isParameter)
  }

  override def toTextFlags(sym: Symbol): Text =
    val symbolFlags = if sym.isType then sym.flags.toTypeFlags else sym.flags.toTermFlags
    toTextFlags(sym, symbolFlags & flagsFilter(sym))

  override def dclText(sym: Symbol): Text =
    toRichDclText(sym.denot)

  override def dclText(d: SingleDenotation): Text =
    toRichDclText(d)


  def toRichText(denot: SingleDenotation): Text =
    val paramss = infoParamssDenotations(denot)
    val evidenceMap = findDesugaredContextBounds(paramss.flatten.map(sym => sym.name -> sym.info))
    val richParamss = enrichParamss(paramss, denot, evidenceMap, false)

    (toText(richParamss) ~ toTextRHS(denot.info.finalResultType)).close

  private def splitExtensionParamss(
    denot: Denotation, paramss: List[List[Param]]
  ): (List[List[Param]], List[List[Param]]) =
    val methodNamePosition = denot.symbol.span.point
    val (extSymbols, methodSymbols) = paramss
      .map: params =>
        params.filter(!_.denot.name.toString.startsWith(ContextBoundParamName.separator))
      .partition:
        case head :: _ => head.denot.symbol.span.end < methodNamePosition
        case Nil => false

    extSymbols -> methodSymbols

  def toRichDclText(denot: SingleDenotation): Text =
    val paramss = infoParamssDenotations(denot)
    val evidenceMap = findDesugaredContextBounds(paramss.flatten.map(sym => sym.name -> sym.info))
    val richParamss = enrichParamss(paramss, denot, evidenceMap, false)
    toRichDclText(denot, richParamss)

  def toRichDclText(denot: SingleDenotation, paramss: List[List[Param]]): Text =
    if denot.symbol.is(ExtensionMethod) then
      val (extSymbols, methodSymbols) = splitExtensionParamss(denot, paramss)

      Str("extension ")
        ~ toText(extSymbols)
        ~ " "
        ~ (toTextFlags(denot.symbol) ~~ keyString(denot.symbol) ~~ nameString(denot.symbol))
        ~ toText(methodSymbols)
        ~ toTextRHS(denot.info.finalResultType).close
    else
      (toTextFlags(denot.symbol) ~~ keyString(denot.symbol) ~~ (varianceSign(denot.symbol.variance) ~ nameString(denot.symbol))
        ~ toText(paramss)
        ~ toTextRHS(denot.info.finalResultType)).close

  /** The name of the symbol without a unique id. */
  protected override def simpleNameString(sym: Symbol): String =
    if sym.name.isConstructorName then nameString(StdNames.nme.this_) else nameString(sym.name)

  /**
   * Create a mapping from type parameter symbol to its context bound.
   *
   * @param paramsWithInfos all parameter names and types of a symbol
   * @return mapping from type param to its context bounds (e.g. Map(T -> List(Ordering[T])) )
   */
  private def findDesugaredContextBounds(paramsWithInfos: List[(Name, Type)]): Map[Name, List[Type]] =
    val evidenceParams = paramsWithInfos.filter(_._1.toString.startsWith(ContextBoundParamName.separator))
    evidenceParams.map(_._2).collect:
      case AppliedType(tycon, (ref: TypeRef) :: Nil)      => (ref.name, tycon) // info from symbol
      case AppliedType(tycon, (ref: TypeParamRef) :: Nil) => (ref.paramName, tycon) // info from tree
    .groupMap(_._1)(_._2)

  private def findDesugaredContextBounds(poly: Type): Map[Name, List[Type]] =
    poly match
      case tpe if tpe.isImplicitMethod =>
        val paramsWithInfos = tpe.paramNamess.flatten zip tpe.paramInfoss.flatten
        findDesugaredContextBounds(paramsWithInfos)

      case methodOrPoly: MethodOrPoly => findDesugaredContextBounds(methodOrPoly.resType)
      case _ => Map.empty


  override def paramsText(lam: LambdaType): Text =
    val result = lam match
      case polyType: PolyType =>
        val evidenceMap = findDesugaredContextBounds(polyType)

        polyType.paramRefs.map: paramRef =>
          val ctxBounds = evidenceMap.getOrElse(paramRef.paramName, Nil).map(toText)
          val ctxBoundsString: Text = Str(": ").provided(ctxBounds.nonEmpty) ~ Text(ctxBounds, ": ")

          ParamRefNameString(paramRef) ~ toTextRHS(paramRef.underlying, isParameter = true) ~ ctxBoundsString
      case lam =>
        lam.paramRefs.map: ref =>
          if ref.paramName.toString.startsWith(ContextBoundParamName.separator) then Text()
          else if lam.isContextualMethod && ref.paramName.toString.startsWith("x$") then toText(ref.underlying)
          else ParamRefNameString(ref) ~ toTextRHS(ref.underlying, isParameter = true)

    Text(result, ", ")

  def enrichParamss(
    paramss: List[List[SingleDenotation]],
    defnDenot: SingleDenotation,
    evidenceMap: Map[Name, List[Type]],
    includeDocs: Boolean
  )(using Context): List[List[Param]] =
    import scala.jdk.CollectionConverters.*

    lazy val defnDoc: Option[SymbolDocumentation] = symbolSearch.symbolDocumentation(defnDenot.symbol)
    lazy val paramDocs: List[SymbolDocumentation] = defnDoc.fold(List.empty): defnDoc =>
      defnDoc.parameters().nn.asScala.toList
    lazy val typeParamDocs: List[SymbolDocumentation] = defnDoc.fold(List.empty): defnDoc =>
      defnDoc.typeParameters().nn.asScala.toList

    def withReplacedJavaNamed(paramDenot: SingleDenotation, index: Int): Text =
      if defnDenot.symbol.flags.is(JavaDefined) && defnDoc.isDefined then
        paramDocs.lift(index).map(_.displayName().nn).getOrElse(nameString(paramDenot.symbol))
      else nameString(paramDenot.symbol)

    def defaultParameter(paramDenot: SingleDenotation, index: Int): Text =
      val hasDefault = paramDenot.symbol.flags.is(HasDefault)
      includeDefaultParam match
        case IncludeDefaultParam.Include if hasDefault && defnDoc.isDefined =>
          paramDocs.lift(index).map(doc => Str(" = ") ~ doc.defaultValue().nn).getOrElse(" = ...")
        case IncludeDefaultParam.ResolveLater if hasDefault => Str(" = ...")
        case _ => ""

    def typeParamsText(syms: List[SingleDenotation]): List[Param] =
      syms
        .zipWithIndex
        .map: (denot, i) =>
          evidenceMap.get(denot.name) match
            case Some(ctxBounds) =>
              val ctxBoundString: Text = Str(": ").provided(ctxBounds.nonEmpty) ~ Text(ctxBounds.map(toText), ": ")
              val text = ParamRefNameString(denot.name) ~ toTextRHS(denot.info, isParameter = true) ~ ctxBoundString
              Param(denot, text.mkString(defaultWidth, false), typeParamDocs.lift(i).map(_.docstring().nn))
            case None =>
              val text = (varianceSign(denot.symbol.variance) ~ nameString(denot.symbol)) ~ toTextRHS(denot.info, isParameter = true).close
              Param(denot, text.mkString(defaultWidth, false), typeParamDocs.lift(i).map(_.docstring().nn))

    def paramsText(syms: List[SingleDenotation]): List[Param] =
      syms
        .filter(!_.name.toString.startsWith(ContextBoundParamName.separator))
        .zipWithIndex
        .map: (denot, i) =>
          val text = if denot.symbol.is(Given) && denot.name.toString.startsWith("x$") then toText(denot.info).close
          else
            withReplacedJavaNamed(denot, i)
            ~ toTextRHS(denot.info, isParameter = true)
            ~ defaultParameter(denot, i).close
          Param(denot, text.mkString(defaultWidth, false), paramDocs.lift(i).map(_.docstring().nn))

    val buf = mutable.ListBuffer.empty[List[Param]]
    paramss.foreach: params =>
      Params.paramsKind(params) match
        case Params.Kind.TypeParameter => buf += typeParamsText(params)
        case Params.Kind.Synthetic     =>
        case _                         => buf += paramsText(params)

    buf.toList


  def toText(paramss: List[List[Param]]): Text =
    val result = paramss.map: params =>
      val paramsText = Text(params.map(_.text), ", ")
      Params.paramsKind(params.map(_.denot)) match
        case Params.Kind.TypeParameter => (Str("[") ~ paramsText ~ Str("]"))
        case Params.Kind.Normal        => (Str("(") ~ paramsText ~ Str(")"))
        case Params.Kind.Using         => (Str("(using ") ~ paramsText ~ Str(")"))
        case Params.Kind.Implicit      => (Str("(implicit ") ~ paramsText ~ Str(")"))
        case Params.Kind.Synthetic     => Str("")
    Text(result, "")

end ShortenedTypePrinter

object ShortenedTypePrinter:

  enum IncludeDefaultParam:
    /** Include default param at `textDocument/completion` */
    case Include

    /**
     * Include default param as "..." and populate it later at `completionItem/resolve`
     * @see https://github.com/scalameta/metals/blob/09d62c2e2f77a63c7d21ffa19971e2bb3fc9ab34/mtags/src/main/scala/scala/meta/internal/pc/ItemResolver.scala#L88-L103
     */
    case ResolveLater

    /** Do not add default parameter */
    case Never

end ShortenedTypePrinter
