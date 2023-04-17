package scala.meta.internal.pc

import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j.CompletionItem
import scala.meta.internal.mtags.CommonMtagsEnrichments._
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import dotty.tools.dotc.util.SourcePosition
import TestExtensions._

object TextEdits {
  def applyEdits(text: String, edits: List[TextEdit]): String = {
    if (edits.isEmpty) text
    else {
      val positions: List[(TextEdit, Range)] = edits
        .map(edit => (edit, Option(edit.getRange)))
        .collect { case (edit, Some(range)) =>
          edit -> range
        }.sortBy((_, range) => (range.getStart.getLine, range.getStart.getCharacter))
      var curr = 0
      val out = new java.lang.StringBuilder()
      positions.foreach { case (edit, pos) =>
        out.append(text, curr, pos.getStart.getOffset(text))
        out.append(edit.getNewText())
        curr = pos.getEnd.getOffset(text)
      }
      out.append(text, curr, text.length)
      out.toString
    }
  }

  def applyEdits(text: String, item: CompletionItem): String = {
    val edits = item.getLeftTextEdit().toList ++
      Option(item.getAdditionalTextEdits).toList.flatMap(_.asScala)
    applyEdits(text, edits)
  }
}

