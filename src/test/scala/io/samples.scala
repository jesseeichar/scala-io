/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


/*************************************************************************************
 * This file contains code samples illustrating how to use the Scala IO API.  It is  *
 * not a test file, other than it is compiled.                                       *
 *                                                                                   *
 *                                                                                   *
 * These examples will be the bases for the tests created for the IO project         *
 *                                                                                   *
 * Note: In order to be more useful all variable have the expected type explicitely  *
 *       declared so that the compiler can detect if there is a problem with the     *
 *       sample code.  It is not required to work (with the exception of the forcing *
 *       the implicits to work in the first two examples                             *
 *************************************************************************************/

object Samples {
  { // implicitly convert strings to paths
    import scalax.io.Path
    import Path.string2path

    val filePath:Path = "/tmp/file"
  }

  { // implicitly convert files to paths
    import java.io.File
    import scalax.io.Path
    import Path.string2path

    val filePath:Path = new File("/tmp/file")
  }

  { // list roots of defaultFileSystem
    import scalax.io.{Path, FileSystem}
    val roots1:List[Path] = Path.roots
    // This method delegates to the defaultFileSystem as follows
    val roots2:List[Path] = FileSystem.defaultFileSystem.roots
  }

  { // Create a Path in the default filesystem explicitly
    import scalax.io.{Path, FileSystem}
    // first use default param to indicate defaultFileSystem
    val path1:Path = Path("/tmp/file1")

    // include windows examples for completeness
    val path2:Path = Path("file://c:/tmp/file2")
    val path3:Path = Path("file://c:\\tmp\\file3")

    //now explicitly state the filesystem
    val path4:Path = Path("/tmp/file4")(FileSystem.defaultFileSystem)

    // or declare an implicit val so it can be reused (this is bot really 
    // required since the default paramter is the default filesystem but 
    // it illustrates how another filesystem can be used
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used to create the path
    val path5:Path = Path("/tmp/file5")

    // a filesystem can also be used to create Paths
    val path6:Path = fs("/tmp/file6")
  }

  { // Create Path from URI
    import scalax.io.{Path}
    import java.net.URI
    // the URI type indicates which filesystem to use
    // file:// indicates the default filesystem
    val path1:Path = Path(new URI("file:///tmp/file1"))

    // include windows examples for completeness
    val path2:Path = Path(new URI("file://c:/tmp/file2"))
    val path3:Path = Path(new URI("file://c:\\tmp\\file3"))

    // For opening a zip filesystem
    val zipPath:Path = Path(new URI("zip:///tmp/zipfile.zip!/file"))
  }

  // TODO demonstrate the GenericPath usage

  { // Create path from java.io.File.
    import java.io.File
    import scalax.io.Path
    // java.io.File are always on the default filesystem
    // so filesystem is not declared
    val path1:Path = Path(new File("/tmp/file1"))
    // include windows examples for completeness
    val path2:Path = Path(new File("file://c:/tmp/file2"))
    val path3:Path = Path(new File("file://c:\\tmp\\file3"))
  }

  { // create temporary files
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named file in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1:Path = Path.makeTempFile()

    // fully declare the temporary file parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files.  
    // The default filesystem does
    val tmpFile2:Path = Path.makeTempFile(prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.defaultFileSystem)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp file
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used by makeTempFile
    val tmpFile3:Path = Path.makeTempFile()

    // a file system can also be used to create temporary files/directories
    fs.makeTempFile()
  }

  { // create temporary directories
    // Note: Both makeTempFile and makeTempDirectory have the same parameters
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named directory in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1:Path = Path.makeTempDirectory()

    // fully declare the temporary directory parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files/directories.  
    // The default filesystem does
    val tmpFile2:Path = Path.makeTempDirectory(prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.defaultFileSystem)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp directory
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used by makeTempDirectory
    val tmpFile3:Path = Path.makeTempDirectory()

    // a file system can also be used to create temporary files/directories
    fs.makeTempFile()
  }

  { // Match a Path against the full path as a string
    import scalax.io.Path
    Path("/tmp/file") match {
      case Path("/tmp/file") => println("it's a match")
      case _ => println("no match")
    }
    Path("/tmp/file") match {
      case Path(stringPath) => println("path as a string is:"+stringPath)
      case _ => println("no match")
    }
  }

  { // demonstrate matching using the matchers that are provided in Path.Matching
    import scalax.io.Path
    import Path.Matching._

    // This example tests if the path is a file, directory, exists or does not exist
    Path("/tmp/file") match {
      case File(file) => println("it's a file!"+file)
      case Directory(dir) => println("it's a directory!"+dir)
      case Exists(path) => println("It exists... but what is it?"+path)
      case NonExistant(path) => println("It does not exist!"+path)
      case _ => println("I give up")
    }

    // Now match based on the permissions of the path
    // Set up matchers we want to use
    import Path.AccessModes._
    val RWE = new AccessMatcher(READ, WRITE, EXECUTE)
    val RW = new AccessMatcher(READ, WRITE)
    val R = new AccessMatcher(READ)
    Path("/tmp/file") match {
      case RWE(path) => println("path is rwe"+path)
      case RW(path) => println("path is rw"+path)
      case R(path) => println("path is r"+path)
    }
  }

  { // Using PathMatcher for matching

    import scalax.io.{Path, PathMatcher, FileSystem}

    // there are three factory methods that matchers
    // Path.matcher (instance method)
    // FileSystem.matcher

    // default type of matcher created is a glob matcher
    val InTmpDir:PathMatcher = Path("/tmp/file").matcher("/tmp/**")

    // If you can also create through the FileSystem
    val InBinDir:PathMatcher = FileSystem.defaultFileSystem.matcher("/bin/*")
    import FileSystem.defaultFileSystem

    // you can explicitly declare the GLOB matcher
    import PathMatcher.StandardSyntax.GLOB
    val StartsWithH:PathMatcher = defaultFileSystem.matcher("**/H*", GLOB)
    
    // a Regex matcher is also available
    import PathMatcher.StandardSyntax.REGEX
    val ContainsVowel:PathMatcher = defaultFileSystem.matcher(".*[aeiou].*", REGEX)

    // If a filesystem supports a filesystem specific sytax you can declare that
    val CustomSyntaxMatcher:PathMatcher = defaultFileSystem.matcher("/tmp/@123", "customSyntax")

    // now demonstrate use
    // See FileSystem.matcher for details on creating a matcher
    Path("/tmp/file") match {
      case InTmpDir(path) => println("Path name is in tmp dir")
      case InBinDir(path) => println("File is in the bin dir")
      case StartsWithH(path) => println("Path name starts with an H")
      case ContainsVowel(path) => println("File contains a vowel")
      case CustomSyntaxMatcher(path) => println("CustomMatcher matched")
    }
  }

  
}

