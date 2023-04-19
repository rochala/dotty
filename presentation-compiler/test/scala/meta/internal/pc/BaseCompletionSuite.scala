package scala.meta.internal.pc

import java.nio.file.Paths
import java.util.Collections
import scala.jdk.CollectionConverters._

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.pc.CancelToken

import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}
import org.junit.Test
import org.junit._
import org.junit.rules.ExpectedException
import org.junit.rules.TestWatcher
import org.junit.runner.Description
import org.junit.rules.RuleChain
import org.junit.rules.TestRule
import org.hamcrest.CoreMatchers._
import org.hamcrest.MatcherAssert
import org.jline.utils.DiffHelper
import dotty.tools.dotc.util.DiffUtil
import org.hamcrest.TypeSafeMatcher
import org.hamcrest
import org.hamcrest.Matcher
import org.hamcrest.StringDescription
import java.awt.image.PackedColorModel

abstract class BaseCompletionSuite extends BasePCSuite:

  private def cancelToken: CancelToken = EmptyCancelToken

  private def resolvedCompletions(
      params: CompilerOffsetParams
  ): CompletionList =
    val result = presentationCompiler.complete(params).get()
    val newItems = result.getItems.asScala.map { item =>
      item.data
        .map { data =>
          presentationCompiler.completionItemResolve(item, data.symbol).get()
        }
        .getOrElse(item)
    }
    result.setItems(newItems.asJava)
    result

  private def getItems(
      original: String,
      filename: String = "A.scala"
  ): Seq[CompletionItem] =
    val (code, offset) = params(original)
    val result = resolvedCompletions(
      CompilerOffsetParams(
        Paths.get(filename).toUri(),
        code,
        offset,
        cancelToken
      )
    )
    result.getItems.asScala
      .sortBy(item => Option(item.getSortText).getOrElse(item.getLabel()))
      .toSeq

  /**
   * Check completions using `fn` returned at @@ cursor position indicated in the `original` string.
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param fn function used to run assertions on completion items
   */
  def checkItems(
      original: String,
      fn: Seq[CompletionItem] => Boolean
  ): Unit =
    assert(fn(getItems(original)))

  /**
   * Check completion line `original`, which if included in the `template` should be
   * changed to `expected` if first completion on the list is applied.
   *
   * @param name name of the tests
   * @param template whole source file with `---` indicating where line `original`
   * should be inserted.
   * @param original line to be inserted and checked
   * @param expected expected line `original` after being modified with the first completion
   * @param filterText filter returned completions according to text
   * @param assertSingleItem make sure only one item is suggested, true by default
   * @param filter similar to filterText, but uses a function
   * @param command additional command that should be applied after this completion is inserted
   */
  def checkEditLine(
      template: String,
      original: String,
      expected: String,
      filterText: String = "",
      assertSingleItem: Boolean = true,
      filter: String => Boolean = _ => true,
      command: Option[String] = None
  ): Unit =
    checkEdit(
      original = template.replace("___", original),
      expected = template.replace("___", expected),
      filterText = filterText,
      assertSingleItem = assertSingleItem,
      filter = filter,
      command = command
    )

  /**
   * Check the results of applying the first completion suggested at a cursor position
   *     indicated by `@@`.
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param expected snippet after applying the first completion
   * @param filterText filter returned completions according to text
   * @param assertSingleItem make sure only one item is suggested, true by default
   * @param filter similar to filterText, but uses a function
   * @param command additional command that should be applied after this completion is inserted
   */
  def checkEdit(
      original: String,
      expected: String,
      filterText: String = "",
      assertSingleItem: Boolean = true,
      filter: String => Boolean = _ => true,
      command: Option[String] = None,
      itemIndex: Int = 0,
      filename: String = "A.scala"
  ): Unit =
    val items =
      getItems(original, filename).filter(item => filter(item.getLabel))

    assertNonEmpty(items, "Obtained empty completions, can't check for edits.")

    if (assertSingleItem && items.length != 1) then
      fail(
        s"expected single completion item, obtained ${items.length} items.\n${items}"
      )

    if (items.size <= itemIndex) then fail(s"Not enough completion items: $items")
    val item = items(itemIndex)
    val (code, _) = params(original)
    val obtained = TextEdits.applyEdits(code, item)

    assertNoDiff(expected, obtained)

    if (filterText.nonEmpty) then
      assertEquals(item.getFilterText, filterText, "Invalid filter text")

    assertEquals(
      command.getOrElse(""),
      Option(item.getCommand).fold("")(_.getCommand),
      "Invalid command"
    )

  /**
   * Check snippet syntax returned in the completions. Snippets show the editor where the cursor should end up ($0).
   *
   * @param name name of the test
   * @param original snippet to test with `@@` indicating the cursor position
   * @param expected string with a list of obtained completions
   */
  def checkSnippet(
      original: String,
      expected: String,
      topLines: Option[Int] = None,
      includeDetail: Boolean = false
  ): Unit =
    val baseItems = getItems(original)
    val items = topLines match
      case Some(top) => baseItems.take(top)
      case None => baseItems

    val obtained = items
      .map { item =>
        val results = item
          .getLeftTextEdit()
          .map(_.getNewText)
          .orElse(Option(item.getInsertText()))
          .getOrElse(item.getLabel)
        if (includeDetail) results + " - " + item.getDetail()
        else results
      }
      .mkString("\n")

    assertCompletions(expected, obtained, Some(original))

  /**
   * Check completions that will be shown in original param after `@@` marker
   * correspoding to the cursor positions
   * @param name test name
   * @param original snippet to test with `@@` indicating the cursor position, by default wrapped in package
   * @param expected expected list of completions
   * @param includeDocs whether to include documentation in the completion description
   * @param includeCommitCharacter  show commit characters, which when typed will
   *    indicate that completion is accepted.
   * @param postProcessObtained function used to modify resulting completion list
   * @param stableOrder we should not sort completions if set to true
   * @param postAssert additional assertions to make on the results
   * @param topLines a number of completions to include, by default all
   * @param filterText filter returned completions according to text
   * @param includeDetail include the completion detail in the results, true by default
   * @param filename name of the file to run the test on, `A.scala` by default
   * @param filter similar to filterText, but uses a function
   * @param enablePackageWrap whether to wrap the code in a package, true by default
   */

  def check(
      original: String,
      expected: String,
      includeDocs: Boolean = false,
      includeCommitCharacter: Boolean = false,
      postProcessObtained: String => String = identity,
      stableOrder: Boolean = true,
      topLines: Option[Int] = None,
      filterText: String = "",
      includeDetail: Boolean = true,
      filename: String = "A.scala",
      filter: String => Boolean = _ => true,
      enablePackageWrap: Boolean = true
  ): Unit =
    val out = new StringBuilder()
    val withPkg =
      if (original.contains("package") || !enablePackageWrap) original
      else s"package test\n$original"
    val baseItems = getItems(withPkg, filename)
    val items = topLines match
      case Some(top) => baseItems.take(top)
      case None => baseItems
    val filteredItems = items.filter(item => filter(item.getLabel))
    filteredItems.foreach { item =>
      val label = TestCompletions.getFullyQualifiedLabel(item)
      val commitCharacter =
        if (includeCommitCharacter)
          Option(item.getCommitCharacters)
            .getOrElse(Collections.emptyList())
            .asScala
            .mkString(" (commit: '", " ", "')")
        else ""
      val documentation = doc(item.getDocumentation)
      if (includeDocs && documentation.nonEmpty) {
        out.append("> ").append(documentation).append("\n")
      }
      out
        .append(label)
        .append({
          val detailIsDefined = Option(item.getDetail).isDefined
          if (
            includeDetail && detailIsDefined && !item.getLabel
              .contains(item.getDetail)
          ) {
            item.getDetail
          } else
            ""
        })
        .append(commitCharacter)
        .append("\n")
    }
    val expectedResult = sortLines(stableOrder, expected)
    val actualResult = sortLines(
      stableOrder,
      postProcessObtained(trimTrailingSpace(out.toString()))
    )

    assertCompletions(expectedResult, actualResult, Some(original))

    if (filterText.nonEmpty) {
      filteredItems.foreach { item =>
        assertEquals(
          item.getFilterText,
          filterText,
          s"Invalid filter text for item:\n$item"
        )
      }
    }

  private def computeDiffMessageEditLines(
      expected: String,
      actual: String
  ): String =
    "\n\n" + DiffUtil.mkColoredCodeDiff(actual, expected, true) + "\n"

  private def trimTrailingSpace(string: String): String =
    string.linesIterator
      .map(_.replaceFirst("\\s++$", ""))
      .mkString("\n")
