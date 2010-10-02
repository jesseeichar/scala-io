package scalax.io

import java.net.URI

/**
 * 
 * User: jeichar
 * Date: Oct 2, 2010
 * Time: 9:31:09 PM
 */

object FileSystemPlugins {
  def lookup(uri:URI) = {
    uri.getScheme match {
      case "file" => Some(Path(uri.getRawPath))
      case "ramfs" => Some(scalax.io.ramfs.RamFileSystem(uri))
      case _ => None
    }
  }
}