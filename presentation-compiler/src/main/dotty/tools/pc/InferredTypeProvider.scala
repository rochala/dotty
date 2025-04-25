package dotty.tools.pc

import java.nio.file.Paths

import scala.annotation.tailrec
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch
import scala.meta as m

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l
import dotty.tools.dotc.transform.CheckUnused.isSynthetic

/**
 * Tries to calculate edits needed to insert the inferred type annotation
 * in all the places that it is possible such as:
 * - value or variable declaration
 * - methods
 * - pattern matches
 * - for comprehensions
 * - lambdas
 *
 * The provider will not check if the type does not exist, since there is no way to
 * get that data from the presentation compiler. The actual check is being done via
 * scalameta parser in InsertInferredType code action.
 *
 * @param params position and actual source
 * @param driver Scala 3 interactive compiler driver
 * @param config presentation compielr configuration
 */
final class InferredTypeProvider(
    params: OffsetParams,
    driver: InteractiveDriver,
    config: PresentationCompilerConfig,
    symbolSearch: SymbolSearch
)(using ReportContext):

  case class AdjustTypeOpts(
      text: String,
      adjustedEndPos: l.Position
  )

  def inferredTypeEdits(
      adjustOpt: Option[AdjustTypeOpts] = None
  ): List[TextEdit] =
    val retryType = adjustOpt.isEmpty
    val uri = params.uri().nn
    val filePath = Paths.get(uri).nn

    val sourceText = adjustOpt.map(_.text).getOrElse(params.text().nn)
    val source =
      SourceFile.virtual(filePath.toString(), sourceText)
    driver.run(uri, source)
    val unit = driver.currentCtx.run.nn.units.head
    val pos = driver.sourcePosition(params)
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

    given locatedCtx: Context = driver.localContext(params)
    val indexedCtx = IndexedContext(locatedCtx)
    val autoImportsGen = AutoImports.generator(
      pos,
      sourceText,
      unit.tpdTree,
      unit.comments,
      indexedCtx,
      config
    )

    def removeType(colonIndex: Int, tptEnd: Int) =
      sourceText.substring(0, colonIndex).nn +
        sourceText.substring(tptEnd + 1, sourceText.length())

    def optDealias(tpe: Type): Type =
      def isInScope(tpe: Type): Boolean =
        tpe match
          case tref: TypeRef =>
            indexedCtx.lookupSym(
              tref.currentSymbol
            ) == IndexedContext.Result.InScope
          case AppliedType(tycon, args) =>
            isInScope(tycon) && args.forall(isInScope)
          case _ => true
      if isInScope(tpe)
      then tpe
      else tpe.deepDealias

    val printer = ShortenedTypePrinter(
      symbolSearch,
      includeDefaultParam = IncludeDefaultParam.ResolveLater,
      isTextEdit = true
    )(using indexedCtx)

    def imports: List[TextEdit] =
      printer.imports(autoImportsGen)

    def printTypeAscription(tpe: Type, addSpaceBefore: Boolean = false, addSpaceAfter: Boolean = false): String =
      (if addSpaceBefore then " : " else ": ") + printer.tpe(tpe) + (if addSpaceAfter then " " else "")

    path.headOption match
      /* `val a = 1` or `var b = 2`
       *     turns into
       * `val a: Int = 1` or `var b: Int = 2`
       *
       *`.map(a => a + a)`
       *     turns into
       * `.map((a: Int) => a + a)`
       */
      case Some(vl @ ValDef(name, tpt, rhs)) =>
        val isParam = path match
          case head :: next :: _ if next.symbol.isAnonymousFunction => true
          case head :: (b @ Block(stats, expr)) :: next :: _
              if next.symbol.isAnonymousFunction =>
            true
          case _ => false

        def baseEdit(withParens: Boolean): TextEdit =
          val keywordOffset = if isParam then 0 else 4

          val editRange = tpt.sourcePos.toLsp
          val addSpaceBefore = name.isOperatorName
          val addSpaceAfter = sourceText(vl.namePos.end) == '='

          adjustOpt.foreach(adjust => editRange.setEnd(adjust.adjustedEndPos))

          new TextEdit(
            editRange,
            printTypeAscription(optDealias(tpt.typeOpt), addSpaceBefore, addSpaceAfter) + {
              if withParens then ")" else ""
            }
          )

        def checkForParensAndEdit(
            applyEndingPos: Int,
            toCheckFor: Char,
            blockStartPos: SourcePosition
        ) =
          val isParensFunction: Boolean = sourceText(applyEndingPos) == toCheckFor

          val alreadyHasParens =
            sourceText(blockStartPos.start) == '('

          if isParensFunction && !alreadyHasParens then
            new TextEdit(blockStartPos.toLsp, "(") :: baseEdit(withParens =
              true
            ) :: Nil
          else baseEdit(withParens = false) :: Nil
        end checkForParensAndEdit

        def typeNameEdit: List[TextEdit] =

          path match
            // lambda `map(a => ???)` apply
            case _ :: _ :: (block: untpd.Block) :: (appl: untpd.Apply) :: _
                if isParam =>
              checkForParensAndEdit(appl.fun.endPos.end, '(', block.startPos)

            // labda `map{a => ???}` apply
            // Ensures that this becomes {(a: Int) => ???} since parentheses
            // are required around the parameter of a lambda in Scala 3
            case valDef :: defDef :: (block: untpd.Block) :: (_: untpd.Block) :: (appl: untpd.Apply) :: _
                if isParam =>
              checkForParensAndEdit(appl.fun.endPos.end, '{', block.startPos)

            case _ =>
              baseEdit(withParens = false) :: Nil

        def lastColon =
          var i = tpt.startPos.start
          while i >= vl.namePos.end && sourceText(i) != ':' do i -= 1
          i

        if tpt.typeOpt.isErroneous || (retryType && !tpt.sourcePos.span.isZeroExtent) then
          val adjustedText = removeType(lastColon, tpt.sourcePos.end - 1)
          inferredTypeEdits(
            Some(AdjustTypeOpts(adjustedText, tpt.sourcePos.toLsp.getEnd().nn))
          )
        else typeNameEdit ::: imports

      /* `def a[T](param : Int) = param`
       *     turns into
       * `def a[T](param : Int): Int = param`
       */
      case Some(df @ DefDef(name, paramss, tpt, rhs: Tree[?])) =>

        def typeNameEdit =
          val editRange = tpt.sourcePos.toLsp
          val addSpaceBefore = name.isOperatorName && paramss.isEmpty
          val addSpaceAfter = sourceText(df.namePos.end) == '='

          adjustOpt.foreach(adjust => editRange.setEnd(adjust.adjustedEndPos))
          val edit = printTypeAscription(optDealias(tpt.typeOpt), addSpaceBefore, addSpaceAfter)

          new TextEdit(editRange, edit)
        end typeNameEdit

        def lastColon =
          var i = tpt.startPos.start
          while i >= df.namePos.end && sourceText(i) != ':' do i -= 1
          i

        if tpt.typeOpt.isErroneous || (retryType && !tpt.sourcePos.span.isZeroExtent) then
          val adjustedText = removeType(lastColon, tpt.sourcePos.end - 1)
          inferredTypeEdits(
            Some(AdjustTypeOpts(adjustedText, tpt.sourcePos.toLsp.getEnd().nn))
          )
        else typeNameEdit :: imports

      /* `case t =>`
       *  turns into
       * `case t: Int =>`
       */
      case Some(bind @ Bind(name, body)) =>
        def baseEdit(withParens: Boolean) =
          val spaceBefore = name.isOperatorName
          new TextEdit(
            bind.endPos.toLsp,
            printTypeAscription(optDealias(body.typeOpt), spaceBefore) + {
              if withParens then ")" else ""
            }
          )
        val typeNameEdit = path match
          /* In case it's an infix pattern match
           * we need to add () for example in:
           * case (head : Int) :: tail =>
           */
          case _ :: (unappl @ UnApply(_, _, patterns)) :: _
              if patterns.size > 1 =>
            val firstEnd = patterns(0).endPos.end
            val secondStart = patterns(1).startPos.start
            val hasDot = params
              .text().nn
              .substring(firstEnd, secondStart).nn
              .exists(_ == ',')
            if !hasDot then
              val leftParen = new TextEdit(body.startPos.toLsp, "(")
              leftParen :: baseEdit(withParens = true) :: Nil
            else baseEdit(withParens = false) :: Nil

          case _ =>
            baseEdit(withParens = false) :: Nil
        typeNameEdit ::: imports

      /* `for(t <- 0 to 10)`
       *  turns into
       * `for(t: Int <- 0 to 10)`
       */
      case Some(i @ Ident(name)) =>
        val spaceBefore = name.isOperatorName
        val typeNameEdit = new TextEdit(
          i.endPos.toLsp,
          printTypeAscription(optDealias(i.typeOpt.widen), spaceBefore)
        )
        typeNameEdit :: imports

      case _ =>
        Nil
    end match
  end inferredTypeEdits

  private def findNamePos(
      text: String,
      tree: untpd.NamedDefTree,
      kewordOffset: Int
  )(using
      Context
  ): SourcePosition =
    val realName = tree.name.stripModuleClassSuffix.toString.toList

    // `NamedDefTree.namePos` is incorrect for bacticked names
    @tailrec
    def lookup(
        idx: Int,
        start: Option[(Int, List[Char])],
        withBacktick: Boolean
    ): Option[SourcePosition] =
      start match
        case Some((start, nextMatch :: left)) =>
          if text.charAt(idx) == nextMatch then
            lookup(idx + 1, Some((start, left)), withBacktick)
          else lookup(idx + 1, None, withBacktick = false)
        case Some((start, Nil)) =>
          val end = if withBacktick then idx + 1 else idx
          val pos = tree.source.atSpan(Span(start, end, start))
          Some(pos)
        case None if idx < text.length() =>
          val ch = text.charAt(idx)
          if ch == realName.head then
            lookup(idx + 1, Some((idx, realName.tail)), withBacktick)
          else if ch == '`' then lookup(idx + 1, None, withBacktick = true)
          else lookup(idx + 1, None, withBacktick = false)
        case _ =>
          None

    val matchedByText =
      if realName.nonEmpty then
        lookup(tree.sourcePos.start + kewordOffset, None, false)
      else None

    matchedByText.getOrElse(tree.namePos)
  end findNamePos

end InferredTypeProvider
