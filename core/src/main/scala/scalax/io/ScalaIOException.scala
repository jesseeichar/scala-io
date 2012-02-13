package scalax.io

import java.io.IOException

case class ScalaIOException(accessException:Option[Throwable], closeExceptions:List[Throwable]) 
        extends IOException({
            val accessExceptionMessage = accessException.map(e => "MainException: "+e.getMessage).getOrElse("No Main Exception") 
            val closingExceptionMessages = closeExceptions.map(_.getMessage()).mkString("\n---\n")
            accessException +"\n---\n"+ closeExceptions
        }) {
    override def getStackTrace() = (accessException.toList ::: closeExceptions).flatMap(_.getStackTrace()).toArray
}