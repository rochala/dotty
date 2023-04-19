package scala.meta.internal.pc

import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.ast.tpd.*

object ScriptFirstImportPosition:

  val usingDirectives: List[String] = List("// using", "//> using")
  val ammHeaders: List[String] = List("// scala", "// ammonite")
