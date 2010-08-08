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
trait OpenOption {
    OpenOption.definedValues = this :: OpenOption.values
}

/**
 * Several options that are supported by most filesystems.
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
object OpenOption {
  private var definedValues = List[OpenOption]()
  def values = definedValues
  /**
   * Append to an existing file.
   * A file will not be created if the file does not exist
   */
   case object Append extends OpenOption
  /**
   * Creating a file if it does not exist, but parent directories will not be created
   */
  case object Create extends OpenOption
  /**
   * Creating a new file and fail if the file already exists
   */
  case object CreateNew extends OpenOption
  /**
   * Creating a new file and all parent directories
   */
  case object CreateFull extends OpenOption
  /**
   * Delete file on close.
   * <p>
   * If this option is used then the best effort will be made
   * to delete the file when close is called.  If close is not called
   * then the file will be deleted on VM termination (if possible)
   * </p>
   */
  case object DeleteOnClose extends OpenOption
  /**
   * Requires that every update to the file's content (but not metadata)
   * be written synchronously to the underlying storage device
   */
  case object DSync extends OpenOption
  /**
   * Open a file for read access
   */
  case object Read extends OpenOption
  /**
   * A hint to create a sparse file if used with {@link #CreateNew}
   */
  case object Sparse extends OpenOption
  /**
   * Requires that every update to the file's content or metadata be
   * written synchronously to the underlying storage device
   */
  case object Sync extends OpenOption
  /**
   * If file exists and is opened for Write access then truncate the file to
   * 0 bytes.  Ignored if opened for Read access.  Truncate takes precedence over 
   * Append.
   */
  case object Truncate extends OpenOption
  /**
   * Open file for write access
   */
  case object Write extends OpenOption

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
  case object NoFollowLinks extends LinkOption() with OpenOption with CopyOption {}
}

/**
 * Flags an option as an option that declares how a file should be copied
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait CopyOption

