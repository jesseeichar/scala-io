/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.Traversable
import StandardOpenOptions._
import IoResource._

// TODO document NotFileException
// TODO Document NoSuchFileException
/**
 * An object for reading and writing files.  FileOperatipns provides access to Channels and streams as well as providing methods for performing simple tasks on files easily.
 * <p>
 * Obtaining a FileOperations from a object does not open a file execution of methods will open a file. Thus it is important to handle NotFileException and FileNotFoundException. Depending on the method one or both exceptions must be handled.
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
  * The API into 3 main sections
  * <ul>
  * <li>Resources - methods for obtaining ManagedResources of input/output streams and byte/file channel. </li>
  * <li>Direct Operations - methods for directly obtaining or writing the contents of the file.</li>
  * <li>Batch Operations - methods for performing several actions on the file in sequence.
  * <p>open() attempts to perform all actions using the open channel in order to improve the performance of the operations.
  * </p><p>
  * lock() performs all the actions using the same channel</li>
  * </ul>
  *
  * @author Jesse Eichar
  * @since 1.0
  */
abstract class FileOperations(override val creationCodec:Codec = Codec.default) extends ReadChars with ReadBytes with WriteChars with WriteBytes {
  /**
   * Obtains an input stream resource for reading from the file
   */
  def inputStream: InputStreamResource
  /**
  * Obtains an OutputStreamResource for writing to the file
  *
  * All {@link StandardOpenOptions} can be used except READ which will be ingnored if present
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           Default is write/create/truncate
  */
  def outputStream(openOptions:OpenOptions*): OutputStreamResource
  /**
   * Obtains a ByteChannel for read/write access to the file.
   *
  * All {@link StandardOpenOptions} can be used
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           Default is read/write/create/truncate
  *
  */
  def channel(openOptions:OpenOptions*): ByteChannelResource
  /**
   * Obtains a FileChannel for read/write access to the file.  Not all filesystems can support FileChannels therefore None will be returned if the filesystem does not support FileChannels.
   *
  * All {@link StandardOpenOptions} can be used
  *
  *  @param openOptions
  *           the options that define how the file is opened when using the stream
  *           Default is read/write/create/truncate
  */
  def fileChannel(openOptions:OpenOptions*): Option[FileChannelResource]

  protected def obtainReadableByteChannel = channel()
  protected def obtainWritableByteChannel = channel()


  def withCodec(codec:Codec): FileOperations

  def patchString(position: Long, 
                  string: String,
                  codec: Codec = getCodec(),
                  openOptions: Iterable[OpenOptions] = List(WRITE)): Unit = {
                    // TODO implement
                    ()
                  }
  def patch(position: Long,
            bytes: Traversable[Byte],
            openOptions: Iterable[OpenOptions] = List(WRITE)): Unit = {
                    // TODO implement
                    ()

            }

  /**
   * Runs several operations as efficiently as possible. If the filesystem permits random access then the same channel will be used to perform all operations.
   * <p>
   * Note: only the direct file operations (bytes,lines,write,patch etc...) can be used and expected to use the same resource. The resource methods all created new streams.
   * </p><p>
   * Note: not all file systems support this, if not then at worst the performance is the same as if they where performed outside an open block
   * </p>
   */
  def open[R](openOptions: Iterable[OpenOptions] = List(WRITE))(action: FileOperations => R): R
                    
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
}

