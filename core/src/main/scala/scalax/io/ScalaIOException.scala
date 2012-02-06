package scalax.io

import java.io.IOException

case class ScalaIOException(allExceptions:List[Throwable]) extends IOException(allExceptions.map(_.getMessage()).mkString("\n---\n")) {
    override def getStackTrace() = allExceptions.flatMap(_.getStackTrace()).toArray
}