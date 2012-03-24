package scalax.file

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
   * Method to implicitly convert a string to a Path
   * object
   */
  implicit def string2path(s: String)(implicit fileSystem: FileSystem = FileSystem.default): Path = fileSystem.fromString(s)
  /**
   * Method to implicitly convert a {@link java.io.File} to a Path
   * object
   */
  implicit def jfile2path(jfile: File): Path = FileSystem.default.fromString(jfile.getPath)

}
