/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.nio.file.{StandardOpenOption => JStandardOpenOption}
/**
 * Several options that are supported by most filesystems.
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
object StandardOpenOption {
  
  /**
   * Append to an existing file.
   * A file will not be created if the file does not exist
   */
   val Append = JStandardOpenOption.APPEND
  /**
   * Creating a file if it does not exist, but parent directories will not be created
   */
  val Create = JStandardOpenOption.CREATE
  /**
   * Creating a new file and fail if the file already exists
   */
  val CreateNew = JStandardOpenOption.CREATE_NEW
  /**
   * Creating a new file and all parent directories
   */
  val CreateFull = new OpenOption{}
  /**
   * Delete file on close.
   * <p>
   * If this option is used then the best effort will be made
   * to delete the file when close is called.  If close is not called
   * then the file will be deleted on VM termination (if possible)
   * </p>
   */
  val DeleteOnClose = JStandardOpenOption.DELETE_ON_CLOSE
  /**
   * Requires that every update to the file's content (but not metadata)
   * be written synchronously to the underlying storage device
   */
  val DSync = JStandardOpenOption.DSYNC
  /**
   * Open a file for read access
   */
  val Read = JStandardOpenOption.READ
  /**
   * A hint to create a sparse file if used with {@link #CreateNew}
   */
  val Sparse = JStandardOpenOption.SPARSE
  /**
   * Requires that every update to the file's content or metadata be
   * written synchronously to the underlying storage device
   */
  val Sync = JStandardOpenOption.SYNC
  /**
   * If file exists and is opened for Write access then truncate the file to
   * 0 bytes.  Ignored if opened for Read access.  Truncate takes precedence over
   * Append.
   */
  val Truncate = JStandardOpenOption.TRUNCATE_EXISTING
  /**
   * Open file for write access
   */
  val Write = JStandardOpenOption.WRITE

  /**
   * Collection of options: {@link #CreateFull}, {@link #Truncate}, {@link #Write}
   */
  final val WriteTruncate = List(CreateFull, Truncate, Write)
  /**
   * Collection of options: {@link #CreateFull}, {@link #Truncate}, {@link #Write}
   */
  final val ReadWrite = List(Read, CreateFull, Write)
  /**
   * Collection of options: {@link #CreateFull}, {@link #Append}, {@link #Write}
   */
  final val WriteAppend = List(CreateFull, Append, Write)
}
