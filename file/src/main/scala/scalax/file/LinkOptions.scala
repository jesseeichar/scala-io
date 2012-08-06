package scalax.file

import scalax.io.OpenOption
import java.nio.file.LinkOption

/**
 * Contains the common Link Options
 *
 * @author Jesse Eichar
 * @since 1.0
 */
object LinkOptions {
  val NoFollowLinks = LinkOption.NOFOLLOW_LINKS
}
