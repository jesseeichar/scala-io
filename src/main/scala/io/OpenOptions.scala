/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
trait OpenOption {}

/**
 * Several options that are supported by most
 * filesystems.
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object OpenOption {
  /**
   * Append to an existing file.
   * A file will not be created if the file does not exist
   */
  final val APPEND = new OpenOption{}
  /**
   * Creating a file if it does not exist
   */
  final val CREATE = new OpenOption{}
  /**
   * Creating a new file and fail if the file already exists
   */
  final val CREATE_NEW = new OpenOption{}
  /**
   * Delete file on close.
   * <p>
   * If this option is used then the best effort will be made
   * to delete the file when close is called.  If close is not called
   * then the file will be deleted on VM termination (if possible)
   * </p>
   */
  final val DELETE_ON_CLOSE = new OpenOption{}
  /**
   * Requires that every update to the file's content (but not metadata)
   * be written synchronously to the underlying storage device
   */
  final val DSYNC = new OpenOption{}
  /**
   * Open a file for read access
   */
  final val READ = new OpenOption{}
  /**
   * A hint to create a sparse file if used with {@link #CREATE_NEW}
   */
  final val SPARSE = new OpenOption{}
  /**
   * Requires that every update to the file's content or metadata be
   * written synchronously to the underlying storage device
   */
  final val SYNC = new OpenOption{}
  /**
   * If file exists and is opened for WRITE access then truncate the file to
   * 0 bytes.  Ignored if opened for READ access
   */
  final val TRUNCATE_EXISTING = new OpenOption{}
  /**
   * Open file for write access
   */
  final val WRITE = new OpenOption{}

  /**
   * Collection of options: {@link #CREATE}, {@link #TRUNCATE_EXISTING}, {@link #WRITE}
   */
  final val WRITE_TRUNCATE = List(CREATE, TRUNCATE_EXISTING, WRITE)
  /**
   * Collection of options: {@link #CREATE}, {@link #APPEND}, {@link #WRITE}
   */
  final val WRITE_APPEND = List(CREATE, APPEND, WRITE)
}

/**
 * Flags an option as an options that declares how to deal with links
 * <p>
 * See LinkOption object for the common options
 * </p>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait LinkOption

/**
 * Contains the common Link Options
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object LinkOption {
  val NOFOLLOW_LINKS = new LinkOption() with OpenOption with CopyOption {}
}

/**
 * Flags an option as an option that declares how a file should be copied
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait CopyOption

