import java.io.File

/**
 * Examples of how to provide custom error handlers when using Scala IO.
 * <p>
 * By default a ScalaIOException is thrown when an error occurs accessing or closing a resource.  The following examples illustrate how
 * this default behaviour can be customized for a specific application.
 * </p>
 */
object ExceptionHandling {
  /**
   * Add a custom error handler that logs an error and returns a default value.
   */
  def logClosingError {
    import scalax.io._

    new ResourceContext {
      override def errorHandler[U](accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        def throwError(t: Throwable) = throw t
        def logClosingErrorAndReturn(result: U) = {
          closingExceptions.foreach(_.printStackTrace())
          result
        }
        accessResult.fold(throwError, logClosingErrorAndReturn)
      }
    }
  }
  /*
  def returnDefaultValue {
    import scalax.io._

    new ResourceContext {
      override def errorHandler[U](accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        val u = implicitly[Manifest[U]].erasure match {
          case str if classOf[String].isInstanceOf[String] => ""
        }
        
        u.asInstanceOf[U]
      }
    }
  }*/
}
