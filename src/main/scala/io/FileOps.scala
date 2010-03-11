/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import scala.collection.Traversable
import OpenOption._
import Resource._
/**
 * An object for reading and writing files.
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
 * @param sourceCodec
 *          the codec that the file was created with
 *
 * @author Jesse Eichar
 * @since 1.0
 */
abstract class BasicFileOps(override val sourceCodec:Codec) extends ReadChars with ReadBytes with WriteChars with WriteBytes {
  
  def withCodec(codec:Codec): BasicFileOps

  /**
   * Update a portion of the file content with string at
   * the declared location.
   * <p>
   * If the position is beyond the end of the file a BufferUnderflow
   * Exception will be thrown
   * </p><p>
   * If the position is within the file but the
   * <code>position + codecConvertedString.getBytes.length</code>
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   * </p><p>
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   * </p>
   * @param position
   *          The start position of the update starting at 0.
   *          The position must be within the file
   * @param string
   *          The string to write to the file starting at
   *          position.
   * @param openOptions
   *          The options to use for opening the file
   *          Default is WRITE
   * @see patch(Long,Traversable[Byte],Iterable[OpenOption])
   */
  def patchString(position: Long, 
                  string: String,
                  codec: Codec = getCodec(),
                  openOptions: Iterable[OpenOption] = List(WRITE)): Unit = {
                    // TODO implement
                    ()
                  }

  /**
   * Update a portion of the file content with several bytes at
   * the declared location.
   * <p>
   * <strong>Important:</strong> The use of an Array is highly recommended
   * because normally arrays can be more efficiently written using
   * the underlying APIs
   * </p>
   * <p>
   * If the position is beyond the end of the file a BufferUnderflow
   * Exception will be thrown
   * </p><p>
   * If the position is within the file but the
   * <code>position + bytes.length</code>
   * is beyond the end of the file the file will be enlarged so
   * that the entire string can fit in the file
   * </p><p>
   * The write begins at the position indicated.  So if position = 0
   * then the write will begin at the first byte of the file.
   * </p>
   * @param position
   *          The start position of the update starting at 0.
   *          The position must be within the file
   * @param bytes
   *          The bytes to write to the file starting at
   *          position.
   * @param openOptions
   *          The options to use for opening the file
   *          Default is WRITE
   */
  def patch(position: Long,
            bytes: Traversable[Byte],
            openOptions: Iterable[OpenOption] = List(WRITE)): Unit = {
     require(position >= 0, "The patch starting position must be within the existing file")
                    // TODO implement
                    ()

            }
}

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
 * @param codec
 *          the codec that the file was created with
 * 
 * @author Jesse Eichar
 * @since 1.0
 */
abstract class FileOps(sourceCodec: Codec) extends BasicFileOps(sourceCodec) {
  def withCodec(codec:Codec): FileOps

  /**
   * Obtains an input stream resource for reading from the file
   */
  def inputStream: InputStreamResource
  /**
  * Obtains an OutputStreamResource for writing to the file
  *
  * All {@link OpenOption} can be used except READ which will be ingnored if present
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           Default is write/create/truncate
  */
  def outputStream(openOptions:OpenOption*): OutputStreamResource
  /**
   * Obtains a ByteChannel for read/write access to the file.
   *
  * All {@link OpenOption} can be used
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           Default is read/write/create/truncate
  */
  def channel(openOptions:OpenOption*): ByteChannelResource
  /**
   * Obtains a FileChannel for read/write access to the file.  Not all filesystems
   * can support FileChannels therefore None will be returned if the filesystem
   * does not support FileChannels.
   *
  * All {@link OpenOption} can be used
  *
  * @param openOptions
  *          the options that define how the file is opened when using the stream
  *          Default is read/write/create/truncate
  */
  def fileChannel(openOptions:OpenOption*): Option[FileChannelResource]

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
   *          Default is WRITE/CREATE
   * @param action
   *          The function that will be executed within the block
   */
  def open[R](openOptions: Iterable[OpenOption] = List(WRITE, CREATE))(action: BasicFileOps => R): R
                    
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
   * The sematics of this locking behavious are very similar to those in the {@link java.nio.channels.FileLock}
   * It is recommended that those javadocs are read and the warnings present in those docs are followed.
   * </p>
   * @param start
   *          the start position of the lock.  Must be a non-negative Long
   * @param size
   *          the length in bits the lock.  If -1 then the entire file from start to the end will be locked
   * @param shared
   *          If true then a shared lock will be obtained if possible.  If shared locks are not supported
   *          then an exclusive lock will be obtained
   *
   * @return the result
   *          the result from the block or None if the filesystem does not support locking
   */
  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Option[R]

  // API ends here.
  protected def obtainReadableByteChannel = channel()
  protected def obtainWritableByteChannel = channel()

  
  /**
   * Execute the file in a separate process if the path
   * is executable.
   *
   * @param arguments
   *          Arguments to send to the process
   * @param configuration
   *          An optional configuration function for configuring
   *          the ProcessBuilder.  The default process builder will
   *          be passed to the function.
   *
   * @return Process
   */
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p=>()):Option[Process]
}

