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
import org.junit.Assert.assertEquals
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
import org.hamcrest.CoreMatchers

trait PcAssertions {

    def assertCompletions(expected: String, actual: String, snippet: Option[String] = None): Unit =
      val longestExpeceted = expected.linesIterator.maxByOption(_.length).map(_.length).getOrElse(0)
      val longestActual = expected.linesIterator.maxByOption(_.length).map(_.length).getOrElse(0)

      val actualMatcher = if longestActual >= 40 || longestExpeceted >= 40 then
        lineByLineDiffMatcher(actual)
      else
        sideBySideDiffMatcher(actual)

      assertThat(expected, actualMatcher, snippet)

    def assertNoDiff(expected: String, actual: String, snippet: Option[String] = None): Unit =
      assertThat(expected, lineByLineDiffMatcher(actual), snippet)

    def assertNonEmpty(actual: Seq[?], message: String, snippet: Option[String] = None): Unit =
      assertWithoutStacktrace(true, actual.nonEmpty, message, snippet)

    def fail(message: String, snippet: Option[String] = None): Nothing =
      val description = new StringDescription

      description.appendText(System.lineSeparator)
      description.appendText(message)
      description.appendText(System.lineSeparator)

      snippet.map(addSnippet(description))

      val error = new AssertionError(description.toString)
      error.setStackTrace(Array.empty)
      throw error

    private def unifyNewlines(str: String): String =
      str.linesIterator.dropWhile(_.isBlank).mkString("\n").stripTrailing()

    private def addSnippet(description: StringDescription)(snippet: String) =
      description.appendText(System.lineSeparator)
      description.appendText("Code snippet:")
      description.appendText(System.lineSeparator)
      description.appendText(System.lineSeparator)
      description.appendText(unifyNewlines(snippet))
      description.appendText(System.lineSeparator)
      description.appendText(System.lineSeparator)

    private def assertWithoutStacktrace[T](expected: T, obtained: T, message: String, snippet: Option[String] = None): Unit =
      if (expected != obtained) then
        val description = new StringDescription

        description.appendText(System.lineSeparator)
        description.appendText(message)
        description.appendText(System.lineSeparator)

        snippet.map(addSnippet(description))

        val error = new AssertionError(description.toString)
        error.setStackTrace(Array.empty)
        throw error

    private def assertThat[T](expected: T, matcher: Matcher[T], snippet: Option[String] = None): Unit =
      val _actual = expected.asInstanceOf[AnyRef]
      if (!matcher.matches(_actual)) then
        val description = new StringDescription

        snippet.map(addSnippet(description))

        description.appendText(System.lineSeparator)
        description.appendText(" (" + Console.GREEN + "+ Expected" + Console.RESET + ", ")
        description.appendText(Console.RED + "- Obtained" + Console.RESET + ", ")
        description.appendText(Console.YELLOW + "DIFF" + Console.RESET + ", ")
        description.appendText("NO CHANGES" + ")")
        description.appendText(System.lineSeparator)

        matcher.describeMismatch(_actual, description)

        val error = new AssertionError(description.toString)
        error.setStackTrace(Array.empty)
        throw error

    private def lineByLineDiffMatcher(expected: String): TypeSafeMatcher[String] =
      new TypeSafeMatcher[String] {

        override def describeMismatchSafely(item: String, mismatchDescription: org.hamcrest.Description): Unit =
          mismatchDescription.appendText(System.lineSeparator)
          mismatchDescription.appendText(DiffUtil.mkColoredHorizontalLineDiff(unifyNewlines(item), unifyNewlines(expected)))
          mismatchDescription.appendText(System.lineSeparator)

        override def describeTo(description: org.hamcrest.Description): Unit = ()
        override def matchesSafely(item: String): Boolean =
          unifyNewlines(expected) == unifyNewlines(item)
      }


    private def sideBySideDiffMatcher(expected: String): TypeSafeMatcher[String] =
      new TypeSafeMatcher[String] {

        override def describeMismatchSafely(item: String, mismatchDescription: org.hamcrest.Description): Unit =
          val cleanedExpected = unifyNewlines(expected)
          val cleanedActual = unifyNewlines(item)

          val expectedLines = cleanedExpected.linesIterator.toSeq
          val actualLines = cleanedActual.linesIterator.toSeq

          mismatchDescription.appendText(DiffUtil.mkColoredLineDiff(expectedLines, actualLines))
          mismatchDescription.appendText(System.lineSeparator)

        override def describeTo(description: org.hamcrest.Description): Unit = ()
        override def matchesSafely(item: String): Boolean =
          unifyNewlines(expected) == unifyNewlines(item)
      }

}
