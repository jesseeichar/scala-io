package scalaio.test
import scalax.io.ResourceContext

class ErrorHandlingTestContext {
    var accessExceptions = 0
    var closeExceptions = 0
    val customContext = new ResourceContext{
      override def errorHandler[U](accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        accessResult.left.toOption foreach {_ => accessExceptions += 1}
        closingExceptions foreach {_ => closeExceptions += 1}
        null.asInstanceOf[U]
      }
    }

}