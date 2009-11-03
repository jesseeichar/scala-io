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
import java.lang.{ Process => JProcess }

object Samples {
  { // implicitly convert strings to paths
    import scalax.io.Path
    import Path.string2path

    val filePath: Path = "/tmp/file"
  }

  { // implicitly convert files to paths
    import java.io.File
    import scalax.io.Path
    import Path.string2path

    val filePath: Path = new File ("/tmp/file")
  }

  { // list roots of defaultFileSystem
    import scalax.io.{Path, FileSystem}
    val roots1: List[Path] = Path.roots
    // This method delegates to the defaultFileSystem as follows
    val roots2: List[Path] = FileSystem.defaultFileSystem.roots
  }

  { // Create a Path in the default filesystem explicitly
    import scalax.io.{Path, FileSystem}
    // first use default param to indicate defaultFileSystem
    val path1: Path = Path ("/tmp/file1")

    // include windows examples for completeness
    val path2: Path = Path ("file://c:/tmp/file2")
    val path3: Path = Path ("file://c:\\tmp\\file3")

    //now explicitly state the filesystem
    val path4: Path = Path ("/tmp/file4")(FileSystem.defaultFileSystem)

    // or declare an implicit val so it can be reused (this is bot really
    // required since the default paramter is the default filesystem but
    // it illustrates how another filesystem can be used
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used to create the path
    val path5: Path = Path ("/tmp/file5")

    // a filesystem can also be used to create Paths
    val path6: Path = fs ("/tmp/file6")
  }

  { // Create Path from URI
    import scalax.io.{Path}
    import java.net.URI
    // the URI type indicates which filesystem to use
    // file:// indicates the default filesystem
    val path1: Path = Path (new URI ("file:///tmp/file1"))

    // include windows examples for completeness
    val path2: Path = Path (new URI ("file://c:/tmp/file2"))
    val path3: Path = Path (new URI ("file://c:\\tmp\\file3"))

    // For opening a zip filesystem
    val zipPath: Path = Path (new URI ("zip:///tmp/zipfile.zip!/file"))
  }

  // TODO demonstrate the GenericPath usage

  { // Create path from java.io.File.
    import java.io.File
    import scalax.io.Path
    // java.io.File are always on the default filesystem
    // so filesystem is not declared
    val path1: Path = Path (new File ("/tmp/file1"))
    // include windows examples for completeness
    val path2: Path = Path (new File ("file://c:/tmp/file2"))
    val path3: Path = Path (new File ("file://c:\\tmp\\file3"))
  }

  { // create temporary files
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named file in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.makeTempFile ()

    // fully declare the temporary file parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files.
    // The default filesystem does
    val tmpFile2: Path = Path.makeTempFile (prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.defaultFileSystem)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp file
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used by makeTempFile
    val tmpFile3: Path = Path.makeTempFile ()

    // a file system can also be used to create temporary files/directories
    fs.makeTempFile ()
  }

  { // create temporary directories
    // Note: Both makeTempFile and makeTempDirectory have the same parameters
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named directory in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.makeTempDirectory ()

    // fully declare the temporary directory parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files/directories.
    // The default filesystem does
    val tmpFile2: Path = Path.makeTempDirectory (prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.defaultFileSystem)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp directory
    implicit val fs = FileSystem.defaultFileSystem
    // fs will now be used by makeTempDirectory
    val tmpFile3: Path = Path.makeTempDirectory ()

    // a file system can also be used to create temporary files/directories
    fs.makeTempFile ()
  }

  { // Match a Path against the full path as a string
    import scalax.io.Path
    Path ("/tmp/file") match {
      case Path ("/tmp/file") => println ("it's a match")
      case _ => println ("no match")
    }
    Path ("/tmp/file") match {
      case Path (stringPath) => println ("path as a string is:"+stringPath)
      case _ => println ("no match")
    }
  }

  { // demonstrate matching using the matchers that are provided in Path.Matching
    import scalax.io.Path
    import Path.Matching._

    // This example tests if the path is a file, directory, exists or does not exist
    Path ("/tmp/file") match {
      case File (file) => println ("it's a file!"+file)
      case Directory (dir) => println ("it's a directory!"+dir)
      case Exists (path) => println ("It exists... but what is it?"+path)
      case NonExistant (path) => println ("It does not exist!"+path)
      case _ => println ("I give up")
    }

    // Now match based on the permissions of the path
    // Set up matchers we want to use
    import Path.AccessModes._
    val RWE = new AccessMatcher (READ, WRITE, EXECUTE)
    val RW = new AccessMatcher (READ, WRITE)
    val R = new AccessMatcher (READ)
    Path ("/tmp/file") match {
      case RWE (path) => println ("path is rwe"+path)
      case RW (path) => println ("path is rw"+path)
      case R (path) => println ("path is r"+path)
    }
  }

  { // Using PathMatcher for matching

    import scalax.io.{Path, PathMatcher, FileSystem}

    // there are three factory methods that matchers
    // Path.matcher (instance method)
    // FileSystem.matcher

    // default type of matcher created is a glob matcher
    val InTmpDir: PathMatcher = Path ("/tmp/file").matcher ("/tmp/**")

    // If you can also create through the FileSystem
    val InBinDir: PathMatcher = FileSystem.defaultFileSystem.matcher ("/bin/*")
    import FileSystem.defaultFileSystem

    // you can explicitly declare the GLOB matcher
    import PathMatcher.StandardSyntax.GLOB
    val StartsWithH: PathMatcher = defaultFileSystem.matcher ("**/H*", GLOB)

    // a Regex matcher is also available
    import PathMatcher.StandardSyntax.REGEX
    val ContainsVowel: PathMatcher = defaultFileSystem.matcher (".*[aeiou].*", REGEX)

    // If a filesystem supports a filesystem specific sytax you can declare that
    val CustomSyntaxMatcher: PathMatcher = defaultFileSystem.matcher ("/tmp/@123", "customSyntax")

    // now demonstrate use
    // See FileSystem.matcher for details on creating a matcher
    Path ("/tmp/file") match {
      case InTmpDir (path) => println ("Path name is in tmp dir")
      case InBinDir (path) => println ("File is in the bin dir")
      case StartsWithH (path) => println ("Path name starts with an H")
      case ContainsVowel (path) => println ("File contains a vowel")
      case CustomSyntaxMatcher (path) => println ("CustomMatcher matched")
    }
  }

  { // common simple Path operations
    // Nothing too fancy here. Coolest is the resolving child
    // files and directories
    import scalax.io.Path
    import java.net.{URI, URL}

    val path: Path = Path ("file")

    // if path is a directory then you can use the /
    // methods to make a new path based on that directory
    val child1: Path = path / "childFile"
    val child2: Path = path / "dir1/f2"
    val child3: Path = path / "dir1" / "f3"
    val child4: Path = path / Path ("f4")
    val child5: Path = path / Path ("dir2") / Path ("f5")

    // the resolve methods is essentially an alias for / for those
    // who are uncomfortable with operator type methods.  Also to
    // maintain a familiar feel with NIO Path
    val child6: Path = path.resolve ("child")
    val child7: Path = path.resolve (Path ("child/grandchild"))

    val name: String = path.name
    val pathString: String = path.path

    // make a Path relative to another path
    // This should result in path "child"
    val relative: Path = path.relativize (Path ("file/child"))

    // There are two ways to query about the access mode of the underlying
    // path object.  One is similar to the java.io.File.  The other is based
    // a single query to test several attributes at once.

    // first the java.io.File way
    val executable: Boolean = path.canExecute
    val readable: Boolean = path.canRead
    val writable: Boolean = path.canWrite

    // next check if file is read and write
    import Path.AccessModes._
    val readWrite: Boolean = path.checkAccess (READ, WRITE)

    // the following are fairly boring queries
    val root: Option[Path] = path.root
    val pathSegments: List[String] = path.segments
    val parent: Option[Path] = path.parent
    val parents: List[Path] = path.parents

    val absolute: Boolean = path.isAbsolute
    val absolutePath: Path = path.toAbsolute
    val uri: URI = path.toURI
    val url: URL = path.toURL

    val exists: Boolean = path.exists
    val notExists: Boolean = path.notExists

    val hidden: Boolean = path.isHidden
    val isSymLink: Boolean = path.isSymlink

    // query last modified information
    val lastModified: Long = path.lastModified
    path.lastModified = System.currentTimeMillis

    val length = path.length

    // A way to test if path is a file/directory without using the matchers
    val isFile: Boolean = path.isFile
    val isDirectory: Boolean = path.isDirectory

    // several simple path comparison queries
    val endsWith: Boolean = path.endsWith (Path ("file"))
    val startsWith: Boolean = path.startsWith (Path ("file"))
    val isSame: Boolean = path.isSame (Path ("file"))
    val isFresher: Boolean = path.isFresher (Path ("/tmp/file"))

    //several lexigraphic comparisons
    val lessThan: Boolean = path < Path("other")
    val lessThanEqual: Boolean = path <= Path("other")
    val greaterThan: Boolean = path > Path("other")
    val greaterThanEqual: Boolean = path >= Path("other")
    val compare: Int = path.compare (Path ("other"))
    val compareTo: Int = path.compareTo (Path ("other"))
  }

  { // create files and directories
    import scalax.io.Path

    val path: Path = Path ("/tmp/file")

    // create file but fail if the file already exists.
    // an exception may be thrown
    path.createFile()

    // force create a file will fail if it is a directory which
    // contain children
    path.createFile(failIfExists=false)

    // TODO createFile with attributes

    // create a directory at the path location
    path.createDirectory()
    path.createDirectory(failIfExists=false)
  }

  { // delete files and directories
    import scalax.io.Path

    val path: Path = Path ("/tmp/file")

    // Will throw IOException if file could not be deleted
    // even if it cannot be deleted because it does not exist
    path.delete()

    // Will not throw exception if file does not exist but will
    // if it is a non-empty directory or not writeable
    path.deleteIfExists()

    // Delete path and all children.  This is currently not a safe method so
    // it should be used with caution.  Future versions will be better
    // by default it will throw an exception if one of the files cannot be deleted
    path.deleteRecursively()

    // Delete path and all children. If a file cannot be deleted then continue on and delete
    // all that can be deleted
    path.deleteRecursively(true)
    // or
    path.deleteRecursively(continueOnFailure=true)
  }

  { // copy and move/rename files
    import scalax.io.Path

    val path: Path = Path ("/tmp/file")
    val dest: Path = Path ("/tmp/file2")

    // make a copy of the file
    // by default this will fail if dest already exists
    // also attribute information like datestamp will be
    // set on the destination file
    // If path is a directory the copy will not be recursive
    path.copyTo (dest)

    // Copy explicitly declaring options
    path.copyTo (target=dest,
                 copyAttributes=false,
                 replaceExisting=true)

    // Move/Rename the path
    // by default throw exception if destination exists
    // and if a copy is required by underlying filesystem then do that
    path.moveTo (target=dest)

    // Here we will overwrite existing files (but not non-empty directories)
    // and will fail if a copy is required (similar to java.io.File.renameTo)
    // if a failure occures an exception is thrown
    path.moveTo (target=dest,
                 replaceExisting=true,
                 atomicMove=true)
  }

  { // execute a file if it is executeable
    import scalax.io.Path
    import scalax.io.Process

    val path: Path = Path ("/tmp/file")

    // attempt to execute the file.  If it is possible then the process will be
    // returned
    val process:Option[Process] = path.execute("arg1", "arg2")


  }

  { // search the contents of a directory and perform operations on the objects encountered

    // This set of examples use the contents method with the partial function parameter
    // there is another way of inspecting directory contents I another example

    import scalax.io.{Path, PathMatcher, DirectoryStream, SecureDirectoryStream}
    import scalax.io.Path.Matching._

    val path:Path = Path("/tmp/")

    // print the name of each object in the directory
    path.directoryStream ().filterEach {case path => println (path.name)}

    // Now print names of each directory
    path.directoryStream ().filterEach {case File(file) => println (file.name)}

    // remove spaces from names of paths
    // renaming with this method can be dangerous because the stream may be calculated lazily on some filesystems and the renamed file could also be processed resulting in a infinite loop
    val ContainsSpace:PathMatcher = path.matcher ("* *")
    path.directoryStream ().filterEach {case ContainsSpace (path) => path.moveTo (Path (path.name.filter (_ != ' ')))}

    // count the number of directories
    val fileCount: Option[Int] = path.directoryStream ().filterFold (0){case (count, File (_)) => count+1}

    // A directory stream can also be constructed with a filter
    // this is sometime preferable because using a PathMatcher as a filter may offer operating system
    // native support for filtering
    // obviously useful when processing directories with many file (millions perhaps)
    // the filter is a function returning a PathMatcher because it is possible to define a
    // directoryStream that traverses many levels of the filesystem tree and the filter
    // function allows a new Matcher to be defined at each level of the tree
    val matcher: PathMatcher = path.matcher("S*")
    path.directoryStream (Some(matcher)).foreach (println _)

    // Also you can attempt to perform atomic operations on a DirectoryStream
    // Since not all filesystems support atomic operations (Non in the pre java 7 implementation)
    // a check must be made to see if a secure directory stream was obtained
    path.directoryStream (lock=true) match  {
      case stream:SecureDirectoryStream[Path]  => stream.foreach (_.delete)
      case _:DirectoryStream[Path] => throw new AssertionError ("This filesystem does not support SecureDirectoryStream!")
    }

  }

  { // Walk the directory tree

    import scalax.io.{Path, PathMatcher, DirectoryStream, SecureDirectoryStream}

    val path:Path = Path("/tmp/")

    // by default only the files contained in the current path are returned but if depth
    // is set (<0 will traverse entire tree) then the stream will visit subdirectories in
    // pre-order traversal

    // search for a .gitignore file down to a depth of 4
    val gitIgnoreRestrictedTree: Option[Path] = path.tree (depth=4).find (_.name == ".gitignore")

    // search for a .gitignore in the entire subtree
    val gitIgnoreFullTree: Option[Path] = path.tree ().find (_.name == ".gitignore")

    // search for the .git directory and println all files from that directory and below up to
    // a depth of 10 and does it on a locked directory

    // this method creates the filters that are used to filter each query for a directories contents
    // origin is the originating path (in this example it is path)
    // relativePath is the path relative from origin to the path that will be contained in the DirectoryStream
    // In this example if the depth == 1 (shown by the length of the relativePath) then only the .git directory is accepted
    // All other directories that are traversed will not be filtered
    def filters (origin:Path, relativePath:Path) = {
      if (relativePath.segments.length > 1) None
      else Some(relativePath.matcher(".git"))
    }

    path.tree (filters,  10, true ) match {
      case stream:SecureDirectoryStream[Path]  => stream.foreach (println _)
      case _:DirectoryStream[Path] => throw new AssertionError ("This filesystem does not support SecureDirectoryStream!")
    }
  }

  { // since the underlying filesystem could change to safely use the DirectoryStream API it is recommended to handle the
    // NotDirectoryException
    import scalax.io.{Path, NotDirectoryException, DirectoryStream}
    import scala.util.control.Exception._

    catching (classOf[NotDirectoryException]) opt {
      Path ("/tmp/dir").directoryStream() map ( _.name)
    } match {
      case None => println ("Not a direcory")
      case Some(names) => println ("files names = "+names)
    }
  }

  { // SecureDirectoryStream examples
    // TODO examples
  }

  
}

