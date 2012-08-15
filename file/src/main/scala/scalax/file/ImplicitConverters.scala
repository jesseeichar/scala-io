package scalax.file

import language.implicitConversions

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
    def asPath(implicit fileSystem: FileSystem = FileSystem.default):Path = ImplicitConversions.string2path(s)(fileSystem)
  }

  /**
   * Method to implicitly add an asPath method to [[java.io.File]]
   */
  implicit def jfileAsPath(jfile: java.io.File) = new {
    def asPath = ImplicitConversions.jfile2path(jfile)
  }
  
  /**
   * Method to implicitly add an asPath method to [[java.nio.file.Path]]
   */
  implicit def jpathAsPath(jpath: java.nio.file.Path) = new {
	  def asPath = ImplicitConversions.jpath2path(jpath)
  }
  /**
   * Method to implicitly add an asJavaFile method to [[scalax.file.Path]]
   */
  implicit def scalaPathAsJFile(path: Path) = new {
    def asJavaFile = ImplicitConversions.scalaPath2jfile(path)
  }
  
  /**
   * Method to implicitly add an asJavaPathMatcher method to [[scalax.file.PathMatcher]]
   */
  implicit def scalaPathMatcherAsJavaPathMatcher(matcher: PathMatcher[Path]) = new {
	  def asJavaPathMatcher = ImplicitConversions.scalaPathMatcher2jpathMatcher(matcher)
  }
  /**
   * Method to implicitly add an asScalaPathMatcher method to [[java.nio.file.PathMatcher]]
   */
  implicit def javaPathMatcherAsScalaPathMatcher(matcher: java.nio.file.PathMatcher) = new {
	  def asScalaPathMatcher = ImplicitConversions.javaPathMatcher2scalaPathMatcher(matcher)
  }
  
  
  /**
   * Method to implicitly add an asScalaFileSystem method to [[java.nio.file.FileSystem]]
   */
  implicit def javaFileSystemAsScalaFileSystem(fs: java.nio.file.FileSystem) = new {
	  def asScalaFileSystem = ImplicitConversions.jfileSystem2FileSystem(fs)
  }
  
  
}
