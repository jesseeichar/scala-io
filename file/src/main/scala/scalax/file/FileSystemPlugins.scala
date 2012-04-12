package scalax.file

import java.net.URI

/**
 *
 * User: jeichar
 * Date: Oct 2, 2010
 * Time: 9:31:09 PM
 */

object FileSystemPlugins {
  def lookup(uri:URI): Option[Path] = {
    uri.getScheme match {
      case "file" => Some(Path.fromString(uri.getRawPath))
      case "ramfs" => Some(scalax.file.ramfs.RamFileSystem(uri))
      case _ => None
    }
  }
}
