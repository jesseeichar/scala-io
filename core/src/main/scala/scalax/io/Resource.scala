/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io._
import java.net.{URL, URLConnection}
import java.nio.channels.{ByteChannel, ReadableByteChannel, WritableByteChannel}

import _root_.resource.{ExtractedEither, ManagedResourceOperations}

import scala.util.control.Exception.allCatch
import scalax.io.CloseAction._
import scalax.io.StandardOpenOption._
import scalax.io.managed._
import scalax.io.nio.SeekableFileChannel
import scalax.io.support.FileUtils

trait OpenedResource[+R] {
  def get:R
  def context:ResourceContext
  def close():List[Throwable] = closeAction(get)
  def closeAction[U >: R]:CloseAction[U]
  def toSingleUseResource = new ManagedResourceOperations[R] {
    def acquireFor[B](f: (R) => B): ExtractedEither[List[Throwable], B] = {
      val result = f(get)
      close() match {
        case Nil => ExtractedEither(Right(result))
        case errors => ExtractedEither(Left(errors))
      }
    }
  }
}
class CloseableOpenedResource[+R <: Closeable](val get:R,val context:ResourceContext, closeResourceAction:CloseAction[R]) extends OpenedResource[R] {
  def closeAction[U >: R] = (closeResourceAction :+ CloseAction ((_:R).close())).asInstanceOf[CloseAction[U]]
}
class UnmanagedOpenedResource[+R](val get:R,val context:ResourceContext) extends OpenedResource[R]{
  def closeAction[U >: R]:CloseAction[U] = CloseAction.Noop
}

/**
 * A trait allowing adding close actions to a Resource.  The purpose of this trait
 * was to allow the correct subclass type to be returned when calling the methods but
 * keeping the Repr type from the main Resource signature
 *
 *
 * @note in standard immutable fashion the methods do not mutate the current object
 * but return a new instance with the modified behaviour
 *
 * @tparam R The type of object that is managed by this resource
 * @tparam Repr The actual type of the concrete subclass
 */
trait ResourceOps[+R, +Repr] {
  /**
   * Get the Resource context associated with this Resource instance.
   *
   * @note as Resources are immutable objects a given Resource instance will always be associated with
   * the same ResourceContext
   *
   * @return the associated ResourceContext
   */
  def context:ResourceContext
  /**
   * Create a Resource instance that is configured with the new ResourceContext
   *
   * @param newContext A new ResourceContext
   *
   * @return a new instance configured with the new context
   */
  def updateContext(newContext:ResourceContext):Repr
  /**
   * Update the current ResourceContext and return a new Resource instance with the updated context
   *
   * @param f A function for transforming the current context to a new context with new values.
   *
   * @return a new instance configured with the new context
   */
  def updateContext(f:ResourceContext => ResourceContext):Repr = updateContext(f(context))
  /**
   * Add a CloseAction that will be executed each time the resource is closed.
   *
   * @param newCloseAction the action to add
   *
   * @return a new resource instance with the close action added
   */
  def addCloseAction(newCloseAction: CloseAction[R]):Repr
}
/**
 * A Resource that can be used to do IO.  Primarily it wraps objects from the java io and makes
 * them more Scala friendly.
 *
 * A common question about Resource is: "Why no simply expose [[scalax.io.Input]],
 * [[scalax.io.Output]], [[scalax.io.Seekable]], [[scalax.io.ReadChars]], [[scalax.io.WriteChars]]?
 * Why do we need a Resource[R] object?"
 *
 * There are several reasons for this.  There are several situations where a developer
 * needs access to the underlying resource.
 *
 * Perhaps they need to interact with a Java API which does not use the [[scalax.io.Input]], etc...
 * APIs.  Another possibility is that the resource may be a specific implementation like PrintStream
 * and they want to use those custom APIs instead of the Scala IO apis.  In that case Resource provides
 * them with the ARM functionality that is very useful.
 *
 * @tparam R
 *          The type of the resource that will be managed by the ManagedResource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Resource[+R] extends ManagedResourceOperations[R] with ResourceOps[R, Resource[R]] {
  self =>
  /**
   * Creates a new instance of the underlying resource (or opens it).
   * Sometimes the code block used to create the Resource is non-reusable in
   * which case this Resource can only be used once.  This is not recommended.
   *
   * When creating a resource it is recommended to pass the code block for creating
   * the resource to the resource so that the resource can be reused.  Of course this
   * is not always possible
   *
   * This method should only be used with care in cases when Automatic
   * Resource Management cannot be used because the
   * {@link InputStream} must be closed manually.
   *
   * This is public only to permit interoperability with certain Java APIs.
   * A better pattern of use should be:
   * {{{
   * resource.acquireFor {
   *   // call java API
   * }
   * }}}
   * or
   * {{{
   * val calculatedResult = resource.acquireAndGet {
   *   // cal java API that returns a result
   * }
   * }}}
   *
   * @note normally the error handler registered with the associated ResourceContext
   *        will handle any errors opening the resource, but when calling this method
   *        the caller must handle any possible errors that are raised.
   * @return the actual resource that has been opened
   */
    def open(): OpenedResource[R]

    /**
     * Open the resource execute the function and either return all errors as a list or the result of the
     * function execution.
     *
     * On open and close error handlers in ResourceContext are called.  If they then raise errors
     * the errors are captured and returned as a Right[List[Throwable]]
     *
     * Perhaps the worst method I have ever written :-(
     */
  final def acquireFor[B](f: R => B): ExtractedEither[List[Throwable], B] = {

      // perhaps the worst written method I have ever done :-(
    val resourceEither = allCatch.either {
      open()
    }
    var closeExceptions: List[Throwable] = Nil

    /** Close resource and assign any exceptions to closeException */
    def close(resource:OpenedResource[R]) = try {
      closeExceptions = resource.close()
    } catch {
      case t: Throwable => closeExceptions = List(t)
    }

    /** Handle error that occurs during resource access */
    def handleAccessError: PartialFunction[Throwable, Either[Throwable, B]] = {
      case c: scala.util.control.ControlThrowable => throw c
      case t => Left(t)
    }

    resourceEither match {
      case Left(t) =>
        ExtractedEither(Left(List(context.openErrorHandler(t))))
      case Right(resource) =>
        val result =
          try Right(f(resource.get))
          catch handleAccessError
          finally close(resource)

        val handleError = result.left.toOption ++ closeExceptions nonEmpty

        if (handleError) {
          try {
            ExtractedEither(Right(context.errorHandler(result, closeExceptions)))
          } catch {
            case t: Throwable => ExtractedEither(Left(List(t)))
          }
        } else {
          ExtractedEither(Right(result.right.get))
        }

    }
  }
}

/**
 * An Resource object that is a also an [[scalax.io.Input]].  This trait adds methods
 * for converting between common io types such as to a [[scalax.io.managed.ReaderResource]] or
 * [[scalax.io.managed.ReadableByteChannelResource]]
 *
 * @tparam R
 *          the type of underlying resource that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait InputResource[+R] extends Resource[R] with Input with ResourceOps[R, InputResource[R]] {

    /**
     * Obtain the [[scalax.io.managed.InputStreamResource]](typically) version of this object.
     *
     * @return the [[scalax.io.managed.InputStreamResource]](typically) version of this object.
     */
  def inputStream: InputResource[InputStream]

  override def copyDataTo(output: Output): Unit =
    output  match {
      case outR: OutputResource[_] =>
        var failedToCopy = false
        for {
          inChan <- this
          outChan <- outR
        } {
          FileUtils.tryCopy(failedToCopy=true)(inChan, outChan)
        }
        if(failedToCopy)
          super.copyDataTo(output)
      case _ => super.copyDataTo(output)
    }

    /**
     * Obtain the [[scalax.io.ReadCharsResource]] version of this object.
     *
     * @param sourceCodec the codec used to convert from bytes to characters
     *
     * @return the [[scalax.io.ReadCharsResource]] version of this object.
     */
    def reader(implicit sourceCodec: Codec = Codec.default) : ReadCharsResource[Reader]
    /**
     * Obtain the [[scalax.io.managed.ReadableByteChannelResource]](typically) version of this object.
     *
     * @return the [[scalax.io.managed.ReadableByteChannelResource]](typically) version of this object.
     */
    def readableByteChannel: InputResource[ReadableByteChannel]
    final def size : Option[Long] = sizeFunc()
    /**
     * Safely calculation the size of the resource or return None in case of failure or if
     * it is not possible to determin size of resource before accessing the resource.
     *
     */
    protected def sizeFunc: () => Option[Long]
}
/**
 * An object that in addition to being a resource is also a [[scalax.io.ReadChars]] Resource.
 *
 * @tparam R The underlying resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait ReadCharsResource[+R] extends Resource[R] with ReadChars with ResourceOps[R, ReadCharsResource[R]]

/**
 * An Resource object that is a also an [[scalax.io.Output]].  This trait adds methods
 * for converting between common io types such as to a [[scalax.io.managed.WriterResource]] or
 * [[scalax.io.managed.WritableByteChannelResource]]
 *
 * @tparam R
 *          the type of underlying resource that is created
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OutputResource[+R] extends Resource[R] with Output with ResourceOps[R, OutputResource[R]] {
  /**
   * Obtain the [[scalax.io.OutputResource]](typically) version of this object.
   *
   * @return the [[scalax.io.OutputResource]](typically) version of this object.
   */
  def outputStream: OutputResource[OutputStream]
  /**
   * Obtain the [[scalax.io.WriteCharsResource]] version of this object.
   *
   * @param sourceCodec the codec used to convert from bytes to characters
   *
   * @return the [[scalax.io.WriteCharsResource]] version of this object.
   */
  def writer(implicit sourceCodec: Codec = Codec.default) : WriteCharsResource[Writer]
  /**
   * Obtain the [[scalax.io.managed.WritableByteChannelResource]](typically) version of this object.
   *
   * @return the [[scalax.io.managed.WritableByteChannelResource]](typically) version of this object.
   */
  def writableByteChannel: OutputResource[WritableByteChannel]
    override def doCopyFrom(input: Input): Unit =

    input match {
      case inR: InputResource[_] =>
        var failedToCopy = false
        for {
          inChan <- inR
            outChan <- this
        } {
          FileUtils.tryCopy(failedToCopy=true)(inChan, outChan)
        }
        if(failedToCopy)
          super.doCopyFrom(input)
      case _ => super.doCopyFrom(input)
    }

}
/**
 * An object that can be viewed as a Seekable object. For example
 * a FileChannel.
 *
 * @tparam R
 *          the type of the underlying Resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait SeekableResource[+R] extends Seekable with InputResource[R] with OutputResource[R] with ResourceOps[R, SeekableResource[R]]

/**
 * An object that in addition to being a resource is also a [[scalax.io.WriteChars]] Resource.
 *
 * @tparam R The underlying resource
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait WriteCharsResource[+R] extends Resource[R] with WriteChars with ResourceOps[R, WriteCharsResource[R]]

/**
 * Defines several factory methods for creating instances of Resource.
 *
 * '''Note:''' It is very important to try an pass a method that creates/opens the underlying resource or
 * the actual creation code as the opener.  This is important so that the resource can
 * be used more than once.  If an opened resource is passed to the factory method the resulting resource can only
 * be used once since it closes the resource when it is closed.
 *
 * '''Example:'''
 *
 * {{{
 *    val URL = new URL("http://scala-lang.org")
 *    val resource: Resource[InputStream] = Resource.fromInputStream(url.openStream).buffered
 * }}}
 *
 * @author  Jesse Eichar
 * @since   1.0
 *
 * @define openDisclaimer  The opener param is a by-name argument an is use to open a new stream.
 * In other words it is important to try and pass in a function for opening
 * the stream rather than the already opened stream so that the returned
 * Resource can be used multiple time
 */
object Resource {
  // InputStream factory methods
  /**
   *  Create an [[scalax.io.managed.InputStreamResource]] from an InputStream or subclass
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new InputStream
   *
   * @return an InputStreamResource
   */
  def fromInputStream[A <: InputStream](opener: => A): InputStreamResource[A] = new InputStreamResource[A](opener)
  /**
   * Create an Output Resource instance from an OutputStream.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new OutputStream
   *
   *
   *
   * @return an OutputStreamResource
   */
  def fromOutputStream[A <: OutputStream](opener: => A) : OutputStreamResource[A] = new OutputStreamResource[A](opener)
  // Reader factory methods
  /**
   * Create an ReadChars Resource instance from an Reader.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new Reader
   *
   * @return an ReaderResource
   */
  def fromReader[A <: Reader](opener: => A) : ReaderResource[A] = new ReaderResource[A](opener)
  // Writer factory methods
  /**
   * Create an WriteChars Resource instance with conversion traits from an Writer.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new Writer
   *
   * @return an WriterResource
   */
  def fromWriter[A <: Writer](opener: => A) : WriterResource[A] = new WriterResource[A](opener)
  // Channel factory methods
  /**
   * Create an Input Resource instance from an ReadableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new ReadableByteChannel
   *
   * @return an ReadableByteChannelResource
   */
  def fromReadableByteChannel[A <: ReadableByteChannel](opener: => A) : ReadableByteChannelResource[A] = new ReadableByteChannelResource[A](opener)
  /**
   * Create an Output Resource instance from an WritableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new WritableByteChannel
   *
   * @return an WritableByteChannelResource
   */
  def fromWritableByteChannel[A <: WritableByteChannel](opener: => A) : WritableByteChannelResource[A] = new WritableByteChannelResource[A](opener)
  /**
   * Create an Input/Output Resource instance from a ByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new ByteChannel
   *
   * @return a ByteChannelResource
   */
  def fromByteChannel[A <: ByteChannel](opener: => A) : ByteChannelResource[A] = new ByteChannelResource[A](opener)
  /**
   * Create an Input/Output/Seekable Resource instance from a SeekableByteChannel.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new SeekableByteChannel
   *
   * @return a SeekableByteChannelResource
   */
  def fromSeekableByteChannel[A <: SeekableByteChannel](opener: => A) : SeekableByteChannelResource[A] = {
    new SeekableByteChannelResource[A](_ => opener,DefaultResourceContext, Noop, seekablesizeFunction(opener),None)
  }

  /**
   * Create an Input/Output/Seekable Resource instance from a function => SeekableByteChannel.  The function
   * takes the open options that describe how the channel should be opened.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new SeekableByteChannel
   *
   * @return a SeekableByteChannelResource
   */
  def fromSeekableByteChannel[A <: SeekableByteChannel](opener: Seq[OpenOption] => A) : SeekableByteChannelResource[A] = {
    new SeekableByteChannelResource[A](opener,DefaultResourceContext, Noop, seekablesizeFunction(opener(Read :: Nil)), None)
  }

  private def seekablesizeFunction(resource: => SeekableByteChannel)= () => {
    val r = resource
    allCatch.opt(try{r.size}finally{r.close()})
  }

  /**
   * Create an Input/Output/Seekable Resource instance from a RandomAccess file.
   *
   * $openDisclaimer
   *
   * @param opener the function for opening a new SeekableByteChannel
   *
   * @return a SeekableByteChannelResource
   */
  def fromRandomAccessFile(opener: => RandomAccessFile) : SeekableByteChannelResource[SeekableFileChannel] = {
    def open = (opts:Seq[OpenOption]) => support.FileUtils.openChannel(opener,opts)
    def sizeFunc = () => allCatch.opt{
      resource.managed(opener).acquireAndGet {
          _.length match {case len if len > -1 => len}
      }
    }
    new SeekableByteChannelResource[SeekableFileChannel](open, new ResourceContext{override def descName=PrefixedName("RandomAccessFile")},Noop, sizeFunc,Some(ReadWrite))
  }

  /**
   * Creates an Input Resource from a URL
   *
   * @param url the url to use for constructing a InputStreamResource
   *
   * @return an InputStreamResource
   */
  def fromURL(url:URL): InputStreamResource[InputStream] = {
    val sizeFunc = () => allCatch.opt {
      val conn: URLConnection = url.openConnection
      try {
        conn.connect()
        conn.getContentLength match {
          case len if len > -1 => len:Long
        }
      } finally {
        conn.getInputStream.close()
      }
    }
    new InputStreamResource(url.openStream,new ResourceContext{override def descName=KnownName(url.toExternalForm)},Noop,sizeFunc)
  }

  /**
   * Converts the string to a URL and creates an Input Resource from the URL
   *
   * @param url the url string to use for constructing a InputStreamResource
   *
   * @return an InputStreamResource
   *
   * @throws java.net.MalformedURLException if the url string is not a valid URL
   */
  def fromURL(url:String): InputStreamResource[InputStream] = fromURL(new URL(url))
  /**
   * Creates a Seekable Resource from a File
   *
   * @param file the file to use for constructing a Seekable Resource
   *
   * @return a SeekableByteChannelResource
   * @throws java.io.IOException if file does not exist
   */
  def fromFile(file:File): SeekableByteChannelResource[SeekableByteChannel] = {
    def open = (opts:Seq[OpenOption]) => support.FileUtils.openChannel(file,opts)
    def sizeFunc = () => allCatch.opt{file.length}
    new SeekableByteChannelResource[SeekableFileChannel](open,new ResourceContext{override def descName=KnownName(file.getPath)},Noop,sizeFunc, None)
  }
  /**
   * Create a file from string then create a Seekable Resource from a File
   *
   * @param file the file to use for constructing a Seekable Resource
   *
   * @return a SeekableByteChannelResource
   * @throws java.io.IOException if file does not exist
   */
  def fromFile(file:String): SeekableByteChannelResource[SeekableByteChannel] =
    fromFile(new File(file))

  /**
   * Create an InputStreamResource from a resource on the classpath.  The classloader from the provided class is used to resolve
   * the resource.
   *
   * An exception is thrown if the resource does not exist
   */
  def fromClasspath(name: String,
                    cl: Class[_] ) : InputStreamResource[InputStream]= {
    val url = cl.getClassLoader.getResource(name)
    require(url != null)
    Resource.fromURL(url)
  }

  /**
   * Create an InputStreamResource from a resource on the classpath.  The current threads context class loader is
   * used to load the resource
   *
   * An exception is thrown if the resource does not exist
   */
  def fromClasspath(name: String) : InputStreamResource[InputStream]= {
    val url = Thread.currentThread.getContextClassLoader.getResource(name)
    require(url != null)
    Resource.fromURL(url)
  }
}

sealed trait ResourceDescName{
  def name:String
}
case class KnownName(name:String) extends ResourceDescName
case class PrefixedName(prefix:String) extends ResourceDescName{
  def name: String = prefix+":"+hashCode
}
case class UnknownName() extends ResourceDescName{
  lazy val name = hashCode.toString
}
