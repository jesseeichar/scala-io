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
object ImplicitConverters {
  /**
   * Method to implicitly add an asPath method to String
   */
  implicit def stringAsPath(s: String) = new {
    def asPath(implicit fileSystem: FileSystem = FileSystem.default):Path = fileSystem.fromString(s)
  }

  /**
   * Method to implicitly convert add an asPath method to [[java.io.File]]
   */
  implicit def jfileAsPath(jfile: File) = new {
    def asPath = FileSystem.default(jfile.getPath)
  }
  
  /**
   * Method to implicitly convert add an asPath method to [[java.nio.file.Path]]
   */
  implicit def jfileAsPath(jfile: java.nio.file.Path) = new {
	  def asPath = FileSystem.default(jfile)
  }

  /**
   * Implicitly convert a an asJavaFile method to [[scalax.file.defaultfs.DefaultPath]]
   */
  implicit def defaultPathAsJFile(path: DefaultPath) = new {
    def asJavaFile = path.jfile.toFile
  }
  
  /**
   * Implicitly convert a an asJFile method to [[scalax.file.defaultfs.DefaultPath]]
   */
  implicit def defaultPathAsJPath(path: DefaultPath) = new {
	  def asJavaPath = path.jfile
  }
}
