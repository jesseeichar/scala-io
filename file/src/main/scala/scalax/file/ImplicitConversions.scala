package scalax.file

import java.io.File
import java.nio.file.{Path => JPath}

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
  implicit def jfile2path(jfile: File): Path = FileSystem.default.fromString(jfile.getPath)
  /**
   * Method to implicitly convert a [[java.nio.file.Path]] to a [[scalax.file.defaultfs.DefaultPath]]
   * object
   */
  implicit def jpath2path(jpath: JPath): Path = FileSystem.default.fromString(jpath.toString)
  /**
   * Implicitly convert a [[scalax.file.defaultfs.DefaultPath]] to a [[java.io.File]]
   */
  implicit def defaultPath2jfile(path:Path): File = path.jpath.toFile
  /**
   * Implicitly convert a [[scalax.file.defaultfs.DefaultPath]] to a [[java.nio.file.Path]]
   */
  implicit def defaultPath2jpath(path:Path): JPath = path.jpath
}
