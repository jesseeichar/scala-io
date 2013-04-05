package scalax.file

import language.implicitConversions
import java.io.File
import java.nio.file.{Path => JPath, PathMatcher => JPathMatcher, Paths => JPaths,
  FileSystem => JFileSystem}

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
   * Method to implicitly convert a [[java.io.File]] to a [[scalax.file.Path]]
   * object
   */
  implicit def jfile2path(jfile: File): Path = jfileSystem2FileSystem(jfile.toPath().getFileSystem()).apply(jfile)
  /**
   * Implicitly convert a [[scalax.file.Path]] to a [[java.io.File]]
   */
  implicit def scalaPath2jfile(path:Path): File = path.jpath.toFile

  /**
   * Method to implicitly convert a [[java.nio.file.Path]] to a [[scalax.file.Path]]
   * object
   */
  implicit def jpath2path(jpath: JPath): Path = jfileSystem2FileSystem(jpath.getFileSystem()).apply(jpath)
  /**
   * Implicitly convert a [[scalax.file.Path]] to a [[java.nio.file.Path]]
   */
  implicit def path2jpath(path:Path): JPath = path.jpath

  /**
   * Implicitly convert a [[scalax.file.FileSystem]] to a [[java.nio.file.FileSystem]]
   */
  implicit def fileSystem2jFileSystem(fs:FileSystem): JFileSystem = fs.jFileSystem
  /**
   * Method to implicitly convert a [[java.nio.file.Path]] to a [[scalax.file.Path]]
   * object
   */
  implicit def jfileSystem2FileSystem(fs: JFileSystem): FileSystem = if(fs == FileSystem.default.jFileSystem) FileSystem.default else new FileSystem(fs)
  
  /**
   * Implicitly convert a [[scalax.file.PathMatcher]] to a [[java.nio.file.PathMatcher]]
   */
  implicit def scalaPathMatcher2jpathMatcher(matcher:PathMatcher[Path]): JPathMatcher = new JPathMatcher {
    override def matches(path: JPath) = matcher(jpath2path(path))
  }
    /**
   * Implicitly convert a [[scalax.file.PathMatcher]] to a [[java.nio.file.PathMatcher]]
   */
  implicit def javaPathMatcher2scalaPathMatcher(matcher:JPathMatcher): PathMatcher[Path] = new PathMatcher.NativePathMatcher(matcher)
}
