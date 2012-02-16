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
      override def errorHandler[A, U](f: A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
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

  /**
   * Illustrate how a default value can be returned if an error occurs.
   * <p>
   * In the example below it is known that only a single request will be made, and the type of result
   * is also known so a default value can be provided.
   * </p><p>
   * This pattern can be quite tricky because the internals of how Scala-IO works needs to be known.
   * For example if providing a default for slurpString then one must know that bytes must be 
   * returned as the default rather than a string.  
   * </p>
   * <p>
   * Also if the scala-io implementation changes the code could break.  Defaults work better when
   * working with acquireAndGet or acquireFor  
   * </p>
   */
  def returnDefaultValueOnFailure {
    import scalax.io._

    // slurpString reads bytes and converts them to a string. so the default value has
    // to be a byte array.
    val default = "Default".getBytes("UTF-8")
    // In this context the default will be returned if there the access fails
    // and errors are logged (stupidly in this example)
    val context = new ResourceContext {
      override def openErrorHandler[A, U](f: A => U, openException: Throwable): U = {
        // log exception
        openException.printStackTrace()
        // need cast because U can be anything
        // but we know our use so we can do this hack
        default.asInstanceOf[U]
      }
      override def errorHandler[A, U](f: A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        closingExceptions.foreach(_.printStackTrace())
        def logAndReturnDefault(t: Throwable) = {
          t.printStackTrace()
          // need cast because U can be anything
          // but we know our use so we can do this hack
          default.asInstanceOf[U]
        }
        accessResult.fold(logAndReturnDefault, result => result)
      }
    }

    // Now the default value should be returned if the resource access throws an exception.
    val string = Resource.fromFile("file").updateContext(context).slurpString
  }

  /**
   * Illustrate an alternate (and advanced) pattern where in the case of an exception a default value is returned by the error handler.
   * <p>
   * This is not a particularly common pattern of usage, but illustrates one of the more advanced possibilities
   * </p>
   */
  def returnDefaultValueWithFunctionMatching {
    import scalax.io._

    /*
     * In the example we are providing a custom type of function that provides a
     * default value, which can be returned in the case of a resource exception
     */
    trait FunctionWithDefault[A, U] extends Function[A, U] {
      def defaultValue: U
    }

    /*
     * Create a custom context that checks if the function is a FunctionWithDefault
     * In this example both openErrorHandler and errorHandler perform the check.
     */
    val context = new ResourceContext {
      override def openErrorHandler[A, U](f: A => U, openException: Throwable): U = {
        f match {
          case f: FunctionWithDefault[_, U] =>
            // perform logging 
            f.defaultValue
          case _ => super.openErrorHandler(f, openException)
        }
      }
      override def errorHandler[A, U](f: A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        f match {
          case f: FunctionWithDefault[_, U] =>
            // perform logging 
            f.defaultValue
          case _ => super.errorHandler(f, accessResult, closingExceptions)
        }
      }
    }

    // the normal Input/Output etc... functions are not FunctionWithDefault objects so
    // we need to call acquireAndGet and pass in out own function for reading the data
    val resource = Resource.fromFile("someFile").reader().updateContext(context)

    // now we can perform the read operation with an instance of our
    // FunctionWithDefault
    resource.acquireAndGet(new FunctionWithDefault[java.io.Reader, String] {
      def defaultValue = "Default"
      def apply(reader: java.io.Reader) = {
        val buffer = new Array[Char](10)
        val read = reader.read(buffer)
        buffer.take(read).mkString
      }
    })
  }
}
