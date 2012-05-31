package scalax.io

import java.io.{PrintStream, PrintWriter, IOException}

object ScalaIOException {
  private def throwableString (t:Throwable) = t.getClass + "("+t.getMessage+")"
}
import ScalaIOException.throwableString
case class ScalaIOException(accessException: Option[Throwable], closeExceptions: List[Throwable])
  extends IOException({
    val m = accessException.map(e => "MainException: " + throwableString(e)).getOrElse("No Main Exception") :: closeExceptions.map(throwableString _) mkString "\n---\n"
    m
  }) {

  lazy val trace = (accessException.toList ++ closeExceptions).flatMap(_.getStackTrace()).toArray

  override def getStackTrace() = trace

  override def getCause = (accessException orElse closeExceptions.headOption).get

  override def toString =
    getMessage

  lazy val fullString =
    getMessage() + trace.mkString ("\n\t", "\n\t", "")
  override def printStackTrace() = println(fullString)

  override def printStackTrace(s: PrintStream) = s.print(fullString)

  override def printStackTrace(s: PrintWriter) = s.print(fullString)
}
