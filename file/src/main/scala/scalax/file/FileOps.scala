/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file

import java.io.{
    InputStream, OutputStream
}
import java.nio.channels.{
    ByteChannel, FileChannel, SeekableByteChannel
}
import java.nio.file.{Files => JFiles}

import scalax.io.support.FileUtils._
import scalax.io._
import processing.SeekableProcessor
import scalax.io.managed._
import scala.collection.Traversable
import scalax.io.StandardOpenOption._
import scalax.io.Resource._
import scalax.io.{Codec, OpenOption, Seekable, LongTraversable}
import java.nio.ByteBuffer

/**
 * An object for reading and writing files.  FileOps provides
 * access to Channels and streams as well as providing methods for
 * performing simple tasks on files easily.
 * <p>
 * Obtaining a FileOps from a object does not open a
 * file execution of methods will open a file. Thus it is important
 * to handle NotFileException and FileNotFoundException. Depending
 * on the method one or both exceptions must be handled.
 * </p><p>
 * Examples of exception handling:
 * <pre><code>
 *  try {
 *   file.lines flatMap _.split(":")
 *  } catch {
 *   case FileNotFoundException => fail
 *   case NotFileException => fail
 *  }
 * </code></pre>
 * or using the Exceptions object
 * <pre><code>
 * import scala.util.control.Exceptions
 * val catcher = catching(classOf[NotFileException], classOf[FileNotFoundException])
 *
 * catcher {
 *   file.lines flatMap _.split(":")
 * }
 * </code></pre>
 *
 * The API into 3 main sections
 * </p>
 * <ul>
 * <li>Resources - methods for obtaining ManagedResources of input/output
 *                 streams and byte/file channel. </li>
 * <li>Direct Operations - methods for directly obtaining or writing
 *                         the contents of the file.</li>
 * <li>Batch Operations - methods for performing several actions
 *                        on the file in sequence.
 *                        <p>
 *                        open() attempts to perform all actions using the
 *                        open channel in order to improve the performance
 *                        of the operations.
 *                        </p><p>
 *                        lock() performs all the actions using the same channel
 *                        <p>
 * </li>
 * </ul>
 *
 *
 * @author Jesse Eichar
 * @since 1.0
 */
abstract class FileOps extends Seekable {
  self : Path =>

  /**
   * Obtains an input stream resource for reading from the file
   *
   * The Resource will be configured with the associated fileSystem's ResourceContext
   */
  def inputStream(): InputStreamResource[InputStream] = {
    Resource.fromInputStream(JFiles.newInputStream(jpath)).updateContext(fileSystem.context)
  }
  /**
  * Obtains an OutputStreamResource for writing to the file
  *
  * The Resource will be configured with the associated fileSystem's ResourceContext
  *
  * All {@link OpenOption} can be used except Read which will be ignored if present
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           The Write option is implicitly added to the set of options
  *           Default is write/create/truncate
  */
  def outputStream(openOptions:OpenOption*) : OutputStreamResource[OutputStream] = {
      val r = openOptions match {
          case Seq() =>
              openOutputStream(jpath,WriteTruncate)
          case opts if opts forall {opt => opt != Write && opt != Append} =>
              openOutputStream(jpath,openOptions :+ Write)
          case _ =>
            openOutputStream(jpath,openOptions)
      }
      r.updateContext(fileSystem.context)
  }
  /**
   * Obtains a ByteChannel for read/write access to the file.  If no OpenOptions are
   * specified the underlying file will be opened with read/write/create/truncate options.
   *
   * The Resource will be configured with the associated fileSystem's ResourceContext
   *
   * All {@link OpenOption} can be used
   *
   *  @param openOptions
   *           the options that define how the file is opened when using the stream
   *           Default is options only
   */
  def channel(openOptions:OpenOption*): SeekableByteChannelResource[SeekableByteChannel] = {
    Resource.fromSeekableByteChannel(openChannel(jpath,openOptions)).updateContext(fileSystem.context)
  }
  /**
   * Obtains a FileChannel for read/write access to the file.  Not all filesystems
   * can support FileChannels therefore None will be returned if the filesystem
   * does not support FileChannels.
   * If no OpenOptions are specified the underlying file will be
   * opened with read/write/create/truncate options
   *
   * All {@link OpenOption} can be used
   *
   * The Resource will be configured with the associated fileSystem's ResourceContext
   *
   * @param openOptions
   *          the options that define how the file is opened when using the stream
   *          Default is read/write/create/truncate
   */
  def fileChannel(openOptions:OpenOption*): Option[SeekableByteChannelResource[FileChannel]] = {
    try {
      jpath.toFile // test that it is a file and therefore has a FileChannel
      Some(Resource.fromSeekableByteChannel(openChannel(jpath,openOptions).asInstanceOf[FileChannel]).updateContext(fileSystem.context))
    } catch {
      case e:UnsupportedOperationException => None
    }
  }

  /**
   * Runs several operations as efficiently as possible. If the filesystem
   * permits random access then the same channel will be used to perform all operations.
   * <p>
   * Note: only the direct file operations (bytes,lines,write,patch etc...)
   * can be used and expected to use the same resource. The resource methods
   * all created new streams.
   * </p><p>
   * Note: not all file systems support this, if not then at worst the performance
   * is the same as if they where performed outside an open block
   * </p>
   *
   * @param openOptions
   *          The options that define how the file is opened for the duration of the
   *          operation
   *          Default is Write/Create/Truncate
   * @param context
   *          The context for controlling buffer sizes error handling and other low level configuration
   *          defaults to filesystem Resource context
   */
  def seekableProcessor(openOptions:Seq[OpenOption] = List(Read,Write), context:ResourceContext = fileSystem.context):SeekableProcessor = {
    new SeekableProcessor(channel(openOptions:_*).open, context)
  }

  /**
   * Performs an operation on the file with a FileLock
   * <p>
   * Not all filesystems support locking.  If not then None will be returned by the method
   * </p>
   * <p>
   * The defaults will lock the entire file with an exclusive lock.  It is possible to modify the lock so that
   * it only locks part of the file and may be a shared lock.  Not all filesystems support shared locks but if that is
   * the case the lock will automatically be upgraded to a exclusiveLock
   * </p>
   * <p>
   * The semantics of this locking behavious are very similar to those in the {@link java.nio.channels.FileLock}
   * It is recommended that those javadocs are read and the warnings present in those docs are followed.
   * </p>
   * @param start
   *          the start position of the lock.  Must be a non-negative Long
   * @param size
   *          the length in bits the lock.  If -1 then the entire file from start to the end will be locked
   * @param shared
   *          If true then a shared lock will be obtained if possible.  If shared locks are not supported
   *          then an exclusive lock will be obtained
   * @param context
   *          The context for controlling buffer sizes error handling and other low level configuration
   *
   * @return the result
   *          the result from the block or None if the filesystem does not support locking
   */
  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false, context:ResourceContext = fileSystem.context)(block: Seekable => R): Option[R] = {
    val self = this
    fileChannel().get.acquireAndGet{ fc =>
      Option(fc.tryLock(start,size,shared)).map{_ => block(self)}
    }
  }

  // API ends here.
  // required for path

  // required methods for Input trait
  override def chars(implicit codec: Codec): LongTraversable[Char] = inputStream().chars(codec)

  protected override def toByteChannelResource():InputResource[ByteChannel] = channel(context.readWriteOpenOptions(classOf[ByteChannel]):_*)/*{
    def resource = {
      val r = channel(Read).open
      r.get.position(0)
      new ByteChannel {
        private[this] val wrapped = r.get
        def resetPosition() = wrapped.position(0)
        def isOpen = wrapped.isOpen
        def close() {r.close()}
        def write(src: ByteBuffer) = wrapped.write(src)
        def read(dst: ByteBuffer) = wrapped.read(dst)
        def size = wrapped.size
      }
    }
    new ByteChannelResource(resource,context, CloseAction.Noop,() => Some(resource.size))
  }*/

  // required method for Output trait
  override protected def underlyingOutput =
    if (canRead) channel(WriteTruncate : _*)
    else outputStream().writableByteChannel
  protected def underlyingChannel(append: Boolean) = {
    if(append) {
      channel(Seq(Append) :_*).open
    } else {
      channel(ReadWrite : _*).open
    }
  }
}
