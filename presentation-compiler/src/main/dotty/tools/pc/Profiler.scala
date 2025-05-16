package dotty.tools.pc

import java.util.logging.Logger
import scala.jdk.CollectionConverters.*
import java.nio.file.Files
import java.nio.file.Path
import com.google.gson.Gson
import dotty.tools.pc.util.TraceEvent

trait Profiler:
  def reportEvent(event: util.TraceEvent): Unit
  def reportEvent(
    name: TraceName,
    cat: String,
    eventPhase: EventPhases,
    ts: Long,
    dur: Long,
    pid: Int,
    tid: Int = Thread.currentThread().nn.getId().toInt,
    args: Seq[String] = Nil
  ): Unit =
    reportEvent(util.TraceEvent(name.toString, cat, eventPhase.ph, ts / 1000, dur / 1000, 1, 1, args.asJava))

  def traced[T](origin: String, id: Int, name: TraceName)(f: => T): T =
    val start = System.nanoTime()
    try f
    finally
      val end = System.nanoTime()
      reportEvent(name, origin, EventPhases.Duration, start, end - start, id)

enum EventPhases(val ph: String):
  case Begin extends EventPhases("B")
  case End extends EventPhases("E")
  case Duration extends EventPhases("X")
  case Instant extends EventPhases("i")

enum TraceName:
  case CancellableTask
  case UncancellableTask
  case Started
  case Timeout
  case Completed
  case CancelRequested
  case CacheHit
  case CacheMiss
  case GracefulCancelTriggered
  case ClientCancelled
  case ServerCancelled
  case CompletedExceptionally
  case Compilation
  case TaskInterruptionRequested
  case TaskInterrupted
  case InterruptCancelTriggered
  case ClientCancelTriggered

case class JsonWritingProfiler(outputFolder: Option[Path]) extends Profiler:
  private val logger: Logger = Logger.getLogger(getClass.getName).nn

  private val outputFolder0 = outputFolder.map(_.resolve(".metals").nn).getOrElse(Files.createTempDirectory("metals-trace").nn)
  private val gson = new Gson()
  private val outputDir = Files.createDirectories(outputFolder0)
  private val outputFile = outputFolder0.resolve("traces.json")

  Files.deleteIfExists(outputFile)
  Files.createFile(outputFile)
  logger.info(s"Writing traces to $outputFile")

  def reportEvent(event: util.TraceEvent): Unit = write(event)


  private def write(event: util.TraceEvent): Unit =
    val json = gson.toJson(event).nn + ",\n"
    Files.write(outputFile, json.getBytes(), java.nio.file.StandardOpenOption.APPEND)

object Profiler:
  def empty: Profiler = new Profiler:
    override def reportEvent(event: TraceEvent): Unit = ()
