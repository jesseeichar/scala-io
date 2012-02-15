package scalax.io

import java.io.{PrintStream, PrintWriter, IOException}


case class ScalaIOException(accessException: Option[Throwable], closeExceptions: List[Throwable])
  extends IOException({
    val accessExceptionMessage = accessException.map(e => "MainException: " + e.getMessage).getOrElse("No Main Exception")
    accessException :: closeExceptions.map(_.getMessage()) mkString "\n---\n"
  }) {

  lazy val trace = (accessException.toList ++ closeExceptions).flatMap(_.getStackTrace()).toArray

  override def getStackTrace() = trace

  override def getCause = (accessException orElse closeExceptions.headOption).get

  override def toString = getMessage

  lazy val fullString =
    getMessage() + trace.mkString ("\n\t", "\n\t", "")
  override def printStackTrace() = println(fullString)

  override def printStackTrace(s: PrintStream) = s.print(fullString)

  override def printStackTrace(s: PrintWriter) = s.print(fullString)
}