/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

/**
 * A flag interface for indicating that the object
 * represents a filesystem dependent option for opening
 * a file. Typically several options are declared together.
 * <p>
 * The {@link OpenOption} object defines
 * several such options that are supported by most
 * filesystems.  The filesystem should define which options
 * are accepted
 * </p>
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
trait OpenOption

/**
 * Several options that are supported by most filesystems.
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
object StandardOpenOption extends Enumeration {

  /**
   * Append to an existing file.
   * A file will not be created if the file does not exist
   */
   val Append = new Val(nextId) with OpenOption
  /**
   * Creating a file if it does not exist, but parent directories will not be created
   */
  val Create = new Val(nextId) with OpenOption
  /**
   * Creating a new file and fail if the file already exists
   */
  val CreateNew = new Val(nextId) with OpenOption
  /**
   * Creating a new file and all parent directories
   */
  val CreateFull = new Val(nextId) with OpenOption
  /**
   * Delete file on close.
   * <p>
   * If this option is used then the best effort will be made
   * to delete the file when close is called.  If close is not called
   * then the file will be deleted on VM termination (if possible)
   * </p>
   */
  val DeleteOnClose = new Val(nextId) with OpenOption
  /**
   * Requires that every update to the file's content (but not metadata)
   * be written synchronously to the underlying storage device
   */
  val DSync = new Val(nextId) with OpenOption
  /**
   * Open a file for read access
   */
  val Read = new Val(nextId) with OpenOption
  /**
   * A hint to create a sparse file if used with {@link #CreateNew}
   */
  val Sparse = new Val(nextId) with OpenOption
  /**
   * Requires that every update to the file's content or metadata be
   * written synchronously to the underlying storage device
   */
  val Sync = new Val(nextId) with OpenOption
  /**
   * If file exists and is opened for Write access then truncate the file to
   * 0 bytes.  Ignored if opened for Read access.  Truncate takes precedence over
   * Append.
   */
  val Truncate = new Val(nextId) with OpenOption
  /**
   * Open file for write access
   */
  val Write = new Val(nextId) with OpenOption

  /**
   * Collection of options: {@link #Create}, {@link #Truncate}, {@link #Write}
   */
  final val WriteTruncate = List(CreateFull, Truncate, Write)
  /**
   * Collection of options: {@link #Create}, {@link #Truncate}, {@link #Write}
   */
  final val ReadWrite = List(Read, CreateFull, Truncate, Write)
  /**
   * Collection of options: {@link #Create}, {@link #Append}, {@link #Write}
   */
  final val WriteAppend = List(CreateFull, Append, Write)
}
