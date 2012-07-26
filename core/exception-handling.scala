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
   * Add a custom error handler that in the case of an error during closing simply logs the error and returns the result that
   * was calculated before closing.
   */
  def logClosingError {
    import scalax.io._

    // Create a new ResourceContext instance that provides a custom errorHandler method
    // Note: the openErrorHandler is not overridden so failure to open resource will use
    // default behaviour
    val context = new ResourceContext {
      override def errorHandler[U](accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        def throwError(t: Throwable) = throw t
        def logClosingErrorAndReturn(result: U) = {
          closingExceptions.foreach(_.printStackTrace())
          result
        }
        accessResult.fold(throwError, logClosingErrorAndReturn)
      }
    }

    // after creating a resource assign context object to the Resource
    Resource.fromFile("someFile").updateContext(context).slurpString

    // Remember Resources are immutable, so a new instance is created by updateContext
    // the following will not work:

    val resourceWithDefaultErrorHandler = Resource.fromFile("someFile")
    val resourceWithCustomErrorHandler = resourceWithDefaultErrorHandler.updateContext(context)
    // resourceWithDefaultErrorHandler != resourceWithDefaultErrorHandler
    // they are 2 different object and have 2 different ResourceContext objects
  }

}
