package scalaio.test
import scalax.io.ResourceContext

class ErrorHandlingTestContext {
    var openExceptions = 0
    var accessExceptions = 0
    var closeExceptions = 0
    val customContext = new ResourceContext{
      override def openErrorHandler[A,U](f: A => U , openException:Throwable): Option[U] = {
        openExceptions += 1
        Some(null.asInstanceOf[U])
      }
      override def errorHandler[A,U](f:A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        accessResult.left.toOption foreach {_ => accessExceptions += 1}
        closingExceptions foreach {_ => closeExceptions += 1}
        accessResult.right.toOption getOrElse null.asInstanceOf[U]
      }
    }

}