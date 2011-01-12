package scalax.file

import scalax.io.OpenOption

/**
 * Flags an option as an options that declares how to deal with links
 * <p>
 * See LinkOption object for the common options
 * </p>
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait LinkOption

/**
 * Contains the common Link Options
 *
 * @author Jesse Eichar
 * @since 1.0
 */
object LinkOption extends Enumeration {
  val NoFollowLinks = new Val(nextId) with LinkOption with OpenOption with CopyOption
}

/**
 * Flags an option as an option that declares how a file should be copied
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait CopyOption
