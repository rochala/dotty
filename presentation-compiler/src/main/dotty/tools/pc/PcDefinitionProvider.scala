package dotty.tools.pc

import java.net.URI
import java.nio.file.Paths
import java.util.ArrayList

import scala.jdk.CollectionConverters.*
import scala.meta.internal.pc.DefinitionResultImpl
import scala.meta.pc.DefinitionResult
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{Exported, ModuleClass}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.Interactive.Include
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.Location
import dotty.tools.dotc.transform.CheckUnused.isSynthetic
import java.nio.file.Path
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.interactive.Interactive.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Flags.*

class PcDefinitionProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    search: SymbolSearch
):

  def definitions(): DefinitionResult =
    definitions(findTypeDef = false)

  def typeDefinitions(): DefinitionResult =
    definitions(findTypeDef = true)
  // FIXME MISSING AUTOIMPORT FOR FLAGS when import was missing

  def contains(tree: untpd.Tree, sourcePos: SourcePosition)(using Context): Boolean = tree match
    case select: untpd.Select =>
      // using `nameSpan` as SourceTree for Select (especially symbolic-infix e.g. `::` of `1 :: Nil`) miscalculate positions
      println(select.nameSpan)
      select.nameSpan.contains(sourcePos.span)
    case tree: untpd.Ident =>
      tree.sourcePos.contains(sourcePos)
    case tree: untpd.NamedDefTree =>
      tree.namePos.contains(sourcePos)
    case tree: NameTree =>
      val z = SourceTree(tree, sourcePos.source)
      z.namePos.contains(sourcePos)
    case _: ImportOrExport => true

    // TODO: check the positions for NamedArg and Import
    case namedArg: untpd.NamedArg =>
      sourcePos.span.end < namedArg.span.start + namedArg.name.asSimpleName.length

    case app: (untpd.Apply | untpd.TypeApply) => contains(app.fun, sourcePos)
    case _ => false
  end contains


  // TODO try to recover from ambigious error
  def findDefinitions(path: List[Tree], pos: SourcePosition, driver: InteractiveDriver): List[SourceTree] = {
    given Context = driver.currentCtx
    val enclTree = path
      .dropWhile(t => !t.symbol.exists && !t.isInstanceOf[NamedArg])
      .headOption
      .getOrElse(EmptyTree)

    println(path)
    println(path.map(_.symbol.source))
    println(enclTree)
    val enclTree0 = if contains(enclTree, pos) then path else Nil

    val includeOverridden = enclTree.isInstanceOf[MemberDef]
    val symbols = enclosingSourceSymbols(enclTree0, pos) // .filter(_.span.contains(pos)) // ++ enclTree.symbol

    val includeExternal = symbols.exists(!_.isLocal)
    val z = Interactive.findDefinitions(symbols, driver, includeOverridden, includeExternal)
    z

  }

  private def definitions(findTypeDef: Boolean): DefinitionResult =
    val uri = params.uri().nn
    val text = params.text().nn
    val filePath = Paths.get(uri)
    driver.run(
      uri,
      SourceFile.virtual(filePath.toString, text)
    )

    given ctx: Context = driver.localContext(params)
    val pos = driver.sourcePosition(params)
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using ctx)

    val pathToUse = Interactive.resolveTypedOrUntypedPath(path, pos)

    // println(Interactive.findDefinitions(path, pos, driver).map(_.tree.symbol))
    // println(Interactive.enclosingSourceSymbols(pathToUse, pos))
    // println("^^^^^^^^^^^^^^^^^^^^^")
    // println()
    // println(untpdPath.take(3))
    // println(untpdPath.head.symbol)

    val definitions = findDefinitions(path, pos, driver).toList
    val syntheticDefinition = Interactive.enclosingTree(path).symbol.sourcePos
    val extra = if syntheticDefinition.isSynthetic || !syntheticDefinition.exists then Nil else List(new Location(syntheticDefinition.source.file.path, syntheticDefinition.toLsp))
    DefinitionResultImpl(
      "",
      (definitions.map(d => new Location(Path.of(d.namePos.source.path).toUri.toString, d.namePos.toLsp)) ++ extra)
        .toSet.toList.asJava
    )

  end definitions

  /**
   * Some nodes might disapear from the typed tree, since they are mostly
   * used as syntactic sugar. In those cases we check the untyped tree
   * and try to get the symbol from there, which might actually be there,
   * because these are the same nodes that go through the typer.
   *
   * This will happen for:
   * - `.. derives Show`
   * @param unit compilation unit of the file
   * @param pos cursor position
   * @return definition result
   */
  private def fallbackToUntyped(pos: SourcePosition, uri: URI)(
    using ctx: Context
  ) =
    lazy val untpdPath = NavigateAST
      .untypedPath(pos.span)
      .collect { case t: untpd.Tree => t }

    definitionsForSymbols(untpdPath.headOption.map(_.symbol).toList, uri, pos)
  end fallbackToUntyped

  private def findDefinitions(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      uri: URI,
  ): DefinitionResult =
    import indexed.ctx
    definitionsForSymbols(
      Interactive.enclosingSourceSymbols(path, pos),
      uri,
      pos
    )
  end findDefinitions

  private def findTypeDefinitions(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      uri: URI,
  ): DefinitionResult =
    import indexed.ctx
    val enclosing = path.expandRangeToEnclosingApply(pos)
    val typeSymbols = MetalsInteractive
      .enclosingSymbolsWithExpressionType(enclosing, pos, indexed)
      .map { case (_, tpe, _) =>
        tpe.typeSymbol
      }
    typeSymbols match
      case Nil =>
        path.headOption match
          case Some(value: Literal) =>
            definitionsForSymbols(List(value.typeOpt.widen.typeSymbol), uri, pos)
          case _ => DefinitionResultImpl.empty
      case _ =>
        definitionsForSymbols(typeSymbols, uri, pos)
  end findTypeDefinitions

  private def definitionsForSymbols(
      symbols: List[Symbol],
      uri: URI,
      pos: SourcePosition
  )(using ctx: Context): DefinitionResult =
    semanticSymbolsSorted(symbols) match
      case Nil => DefinitionResultImpl.empty
      case syms @ ((_, headSym) :: tail) =>
        val locations = syms.flatMap:
          case (sym, semanticdbSymbol) =>
            locationsForSymbol(sym, semanticdbSymbol, uri, pos)
        DefinitionResultImpl(headSym, locations.asJava)

  private def locationsForSymbol(
      symbol: Symbol,
      semanticdbSymbol: String,
      uri: URI,
      pos: SourcePosition
  )(using ctx: Context): List[Location] =
    val isLocal = symbol.source == pos.source
    if isLocal then
      val trees = driver.openedTrees(uri)
      val include = Include.definitions | Include.local
      val (exportedDefs, otherDefs) =
        Interactive.findTreesMatching(trees, include, symbol)
          .partition(_.tree.symbol.is(Exported))
      otherDefs.headOption.orElse(exportedDefs.headOption).collect:
        case srcTree if srcTree.namePos.exists =>
          new Location(params.uri().toString(), srcTree.namePos.toLsp)
      .toList
    else search.definition(semanticdbSymbol, uri).asScala.toList

  def semanticSymbolsSorted(
      syms: List[Symbol]
  )(using ctx: Context): List[(Symbol, String)] =
    syms
      .collect { case sym if sym.exists =>
        // in case of having the same type and teerm symbol
        // term comes first
        // used only for ordering symbols that come from `Import`
        val termFlag =
          if sym.is(ModuleClass) then sym.sourceModule.isTerm
          else sym.isTerm
        (termFlag, sym.sourceSymbol, SemanticdbSymbols.symbolName(sym))
      }
      .sortBy { case (termFlag, _, name) => (termFlag, name) }
      .map(_.tail)

end PcDefinitionProvider
