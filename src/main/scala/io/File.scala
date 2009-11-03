/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile}
import java.net.{ URI, URL }
import collection.{Traversable}

abstract class File(override val creationCodec:Codec = Codec.default) extends Chars {
  def outputStream: OutputStreamResource
  def channel: ByteChannelResource
  def fileChannel: Option[FileChannelResource]

  def withCodec(codec:Codec): File

  def writeString(string: String,
                  append: Boolean = false,
                  codec: Codec = creationCodec): Unit = {
    // TODO
    ()
  }
  def writeBytes(bytes: Traversable[Byte],
                 append: Boolean = false): Unit = {
    // TODO
    ()
  }
  def writeStrings(strings: Traversable[String],
                   append: Boolean = false,
                   codec: Codec = creationCodec): Unit = {
    // TODO
    ()
  }
  def writeLines(strings: Traversable[String],
                 terminiator: String = compat.Platform.EOL,
                 append: Boolean = false,
                 codec: Codec = creationCodec): Unit = {
    // TODO
    ()
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
  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Some[R]
}

