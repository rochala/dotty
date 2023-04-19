package scala.meta.internal.pc

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.util.control.NonFatal

import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.pc.CancelToken
import dotty.tools.dotc.semanticdb.DescriptorParser

abstract class BaseCodeActionSuite extends BasePCSuite:

  def cancelToken: CancelToken = EmptyCancelToken

  def params(
      code: String
  ): (String, String, Int) =
    val filename = "test.scala"
    val targetRegex = "<<(.+)>>".r
    val target = targetRegex.findAllMatchIn(code).toList match
      case Nil => fail("Missing <<target>>")
      case t :: Nil => t.group(1)
      case _ => fail("Multiple <<targets>> found")
    val code2 = code.replace("<<", "").replace(">>", "")
    val offset = code.indexOf("<<") + target.length()
    val file = tmp.resolve(filename)
    (code2, target, offset)
