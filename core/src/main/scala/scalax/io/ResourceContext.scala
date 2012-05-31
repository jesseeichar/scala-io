package scalax.io
import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.Channel
import java.nio.channels.FileChannel

/**
 * The context a resource uses for obtaining configuration information.  This
 * object is in large part a configuration object for a resource.  It controls the size and
 * buffer type used to read data to and from  channels.  It controls how errors are handled, etc...
 *
 */
trait ResourceContext {
  self =>

  /**
   * The recommended size of a byte buffer for the current platform
   */
  final val recommendedByteBufferSize = 4 * 1024
  /**
   * The recommended size of a character buffer for the current platform
   */
  final val recommendedCharBufferSize = 1024

  /**
   * Called when opening the resource and an exception occurs.
   *
   * The default behaviour is to throw the openException
   *
   * @note The error handling is very experimental and could be removed or dramatically altered.  Please give feedback.  An error handling API less likely to change is the Processor API error handling.
   *
   * @param f the function that would have been executed if the resource opened correctly.
   *   Can be used as context to decide how to handle the error
   * @param openException the exception that was raised while opening the resource.
   *
   * @return if a default value can be determined by inspecting f or openException then this
   *         method can return that value.  The value will be returned in lieu of the
   *         result calling f.   If a value cannot be returned an exception should be thrown.  DO NOT RETURN null!
   */
  def openErrorHandler[A,U](f: A => U , openException:Throwable):Option[U] = throw openException
  /**
   * Called when an exception is raised during an IO operation.  The resource will be closed and all exceptions (including the closing exceptions)
   * will be passed to this errorHandler.
   *
   * If no exception occurred during the access of the resource the mainException will be a Right(...) containing the result and the closingExceptions
   * will contain the list of exceptions raised while closing the resource (and the CloseActions)
   *
   * The default behaviour is to throw a ScalaIOException irregardless of whether the exception occurred during the operation or during closing the
   * resource.
   *
   * @note The error handling is very experimental and could be removed or dramatically altered.  Please give feedback.  An error handling API less likely to change is the Processor API error handling.
   *
   * @param accessResult the exception that occurred during the resource access or the result.
   *
   * @param closingExceptions the exceptions raised while closing the resource.
   *
   * @return the value of accessResult if it has a result or optionally a default value if a default value can be determined from problemFunction or
   * the accessResult (possibly the resulting error).  If a value cannot be returned an exception should be thrown.  DO NOT RETURN null!
   */
  def errorHandler[A,U](problemFunction:A => U, accessResult: Either[Throwable,U], closingExceptions: List[Throwable]):U =
    throw new ScalaIOException(accessResult.left.toOption, closingExceptions)

  /**
   * A name that describes the resource.  This has no real functional value, it is intended to assist in debugging (for example loggin)
   */
  def descName: ResourceDescName = UnknownName()
  /**
   * Returns the size of the buffer to create.  If the dataSize is non-null it can be used in determining the best size to use.
   *
   * The default algorithm will create a buffer that is dataSize if dataSize <= recommendedByteBufferSize and the buffer is read-only.
   * Otherwise recommendedByteBufferSize is used
   *
   * @param dataSize optionally provide the size of the input.  usually only matters when reading
   * @param readOnly indicates if the buffer will be used in read only operations
   */
  def byteBufferSize(dataSize: Option[Long], readOnly: Boolean): Int = dataSize match {
    case Some(size) if size > 0 && readOnly => (size min recommendedByteBufferSize).toInt
    case _ => recommendedByteBufferSize
  }
  /**
   * Returns the size of the character buffer to create.  If the dataSize is non-null it can be used in determining the best size to use.
   *
   * The default algorithm will create a buffer that is dataSize if dataSize <= recommendedCharBufferSize and the buffer is read-only.
   * Otherwise recommendedCharBufferSize is used
   *
   * @param dataSize optionally provide the size of the input.  usually only matters when reading
   * @param readOnly indicates if the buffer will be used in read only operations
   */
  def charBufferSize(dataSize: Option[Int], readOnly: Boolean): Int = dataSize match {
    case Some(size) if size > 0 && readOnly => size min recommendedCharBufferSize
    case _ => recommendedCharBufferSize
  }
  /**
   * Creates a buffer for channels.  This is java specific API and is not cross platform.  Subclasses override this method in order to
   * enable memory mapping of files.
   *
   * The default implementation will create a direct ByteBuffer unless
   * <ol>
   *    <li>the channel is a filechannel</li>
   *    <li>the channel size is smaller than bufferSize</li>
   *    <li>It is in read-only mode</li>
   * </ol>
   * If all these criteria are met then the file will be memory-mapped.
   *
   * @param bufferSize size of the buffer to make.  The buffer will always be this size, unless the file is memory-mapped
   *                   and the channel is smaller than bufferSize.  In which case it will be the size of the channel.
   * @param channel Optionally the channel that this buffer will be used with.  See section above about memory mapping.
   *                The channel can be used by subclasses to determine the type of buffer to return
   * @param readOnly if the use of the buffer will be readonly on the channel provided
   */
  def createNioBuffer(bufferSize: Int, channel: Option[Channel], readOnly: Boolean): java.nio.ByteBuffer = channel match {
    case Some(channel: FileChannel) if readOnly && channel.size <= bufferSize =>
      channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size)
    case _ => ByteBuffer.allocateDirect(bufferSize)
  }
  /**
   *
   * Creates the byte buffer.
   *
   * Implementation is simply: {{{createNioBuffer(byteBufferSize(dataSize, readOnly), channel, readOnly)}}}
   *
   * @param dataSize size of the buffer to make.  The buffer will always be this size, unless the file is memory-mapped
   *                   and the channel is smaller than bufferSize.  In which case it will be the size of the channel.
   * @param channel Optionally the channel that this buffer will be used with.  See section above about memory mapping.
   *                The channel can be used by subclasses to determine the type of buffer to return
   * @param readOnly if the use of the buffer will be read-only on the channel provided
   */
  final def createNioBuffer(dataSize: Option[Long], channel: Option[Channel], readOnly: Boolean): java.nio.ByteBuffer = createNioBuffer(byteBufferSize(dataSize, readOnly), channel, readOnly)

  /**
   * Mutate the current instance by modifying the behavior of some of the functions.
   *
   * A typical use is as follows:
   *
   * {{{
   * context.copy(newErrorHandler = Some( myErrorHandler ) )
   * }}}
   *
   * This example will return a new copy of the context with a new error handler configured.
   * All other methods will behave the same as the original context.
   *
   * @param newByteBufferSize A new strategy for determining the size of a byte buffer to make.
   *        The default value is None which will keep the behaviour of the current context
   * @param newCharBufferSize A new strategy for determining the size of a character buffer to make
   *        The default value is None which will keep the behaviour of the current context
   * @param newCreateNioBuffer A new strategy for creating an nio ByteBuffer
   *        The default value is None which will keep the behaviour of the current context
   * @param newErrorHandler A new strategy for handling errors encountered by the associated resource
   *        The default value is None which will keep the behaviour of the current context
   * @param newDescName A new descriptive name for the associated resources
   *        The default value is None which will keep the behaviour of the current context
   */
  def copy[A,U](
    newByteBufferSize: Option[(Option[Long], Boolean) => Int] = None,
    newCharBufferSize: Option[(Option[Int], Boolean) => Int] = None,
    newCreateNioBuffer: Option[(Int, Option[Channel], Boolean) => ByteBuffer] = None,
    newOpenErrorHandler: Option[(A => U, Throwable) => Option[U]] = None,
    newErrorHandler: Option[(A => U, Either[Throwable,U], List[Throwable]) => U] = None,
    newDescName: Option[ResourceDescName] = None) = new ResourceContext {
    override def errorHandler[A2,U2](problemFunction:A2 => U2, accessResult: Either[Throwable,U2], closingExceptions: List[Throwable]): U2 = {
       newErrorHandler match {
         case Some(handler) =>
           val castFunction = problemFunction.asInstanceOf[A => U]
           val castResult = accessResult.asInstanceOf[Either[Throwable,U]]
           handler(castFunction,castResult, closingExceptions).asInstanceOf[U2]
         case None =>
           self.errorHandler(problemFunction, accessResult, closingExceptions)
       }
    }
    override def openErrorHandler[A2,U2](f: A2 => U2 , openException:Throwable): Option[U2] = {
      newOpenErrorHandler match {
        case Some(handler) =>
            val castFunction = f.asInstanceOf[A => U]
            handler(castFunction, openException).asInstanceOf[Option[U2]]
        case None =>
          self.openErrorHandler(f,openException)

      }
    }
    override def descName: ResourceDescName = newDescName getOrElse self.descName
    override def byteBufferSize(dataSize: Option[Long], readOnly: Boolean): Int = (newByteBufferSize getOrElse (self.byteBufferSize _))(dataSize, readOnly)
    override def charBufferSize(dataSize: Option[Int], readOnly: Boolean): Int = (newCharBufferSize getOrElse (self.charBufferSize _))(dataSize, readOnly)
    override def createNioBuffer(bufferSize: Int, channel: Option[Channel], readOnly: Boolean): java.nio.ByteBuffer =
      newCreateNioBuffer.map(_(bufferSize, channel, readOnly)) getOrElse self.createNioBuffer(bufferSize, channel, readOnly)
  }
}

/**
 * The default ResourceContext
 */
object DefaultResourceContext extends ResourceContext
