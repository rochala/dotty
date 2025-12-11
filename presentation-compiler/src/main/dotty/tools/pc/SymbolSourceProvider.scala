package dotty.tools.pc

import java.nio.file.Paths

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.utils.InteractiveEnrichments.*

import scala.meta.pc.SymbolSource
import org.slf4j.Logger
import dotty.tools.io.FileExtension
import dotty.tools.dotc.core.Flags

class SymbolSourceProvider(driver: InteractiveDriver, params: OffsetParams):

  val logger: Logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  def source(): List[SymbolSource] =
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
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

    Interactive.enclosingSourceSymbols(path, pos).map : symbol =>
      symbol.associatedFile.nn.ext match
        case FileExtension.Tasty | FileExtension.Betasty =>
          val jar = symbol.associatedFile.nn.underlyingSource
          val directPath = symbol.associatedFile.nn.absolutePath
          val range = symbol.sourcePos.toLsp

          jar match
            case Some(jar) => // Direct path is in jar path to the tasty file
              SymbolSource.ExternalTastySymbolSource(jar.absolutePath, directPath, symbol.showFullName, range)
            case _ => // There is no jar, so it means we have direct access to source file
              SymbolSource.InternalTastySymbolSource(directPath, symbol.source.path, symbol.showFullName, range)

        case FileExtension.Class =>
          val jar = symbol.associatedFile.nn.underlyingSource
          val directPath = symbol.associatedFile.nn.absolutePath
          val isJava = symbol.is(Flags.JavaDefined)

          jar match
            case Some(jar) => // Direct path is in jar path to the class file
              SymbolSource.ExternalClassFileSymbolSource(jar.absolutePath, directPath, symbol.showFullName, isJava)
            case _ => // There is no jar, so it means we have direct access to source file // czyli nie ma range nawet jak jest lokalny plik
              SymbolSource.InternalClassFileSymbolSource(directPath, symbol.showFullName, isJava)

        case FileExtension.Scala =>
          val directPath = symbol.associatedFile.nn.absolutePath
          val range = symbol.sourcePos.toLsp
          SymbolSource.ScalaFileSymbolSource(directPath, symbol.showFullName, range)

        case _ => scala.sys.error(s"Unsupported file extension ${symbol.associatedFile.nn.ext} for symbol ${symbol.name} and source ${symbol.associatedFile.nn.path}")

end SymbolSourceProvider
