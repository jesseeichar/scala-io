package scalax.file

import defaultfs.DefaultPath
import java.io.File

/**
 * Contains the implicit conversion methods for converting to Paths and related objects
 *
 * User: jeichar
 * Date: 3/24/12
 * Time: 7:26 PM
 */
object ImplicitConversions {
  /**
   * Method to implicitly convert a string to a [[scalax.file.Path]]
   * object
   */
  implicit def string2path(s: String)(implicit fileSystem: FileSystem = FileSystem.default): Path = fileSystem.fromString(s)
  /**
   * Method to implicitly convert a [[java.io.File]] to a [[scalax.file.defaultfs.DefaultPath]]
   * object
   */
  implicit def jfile2path(jfile: File): DefaultPath = FileSystem.default.fromString(jfile.getPath)
  /**
   * Implicitly convert a [[scalax.file.defaultfs.DefaultPath]] to a [[java.io.File]]
   */
  implicit def defaultPath2jfile(path:DefaultPath): File = path.jfile
}
