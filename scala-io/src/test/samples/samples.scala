/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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
  { // create temporary files
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named file in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.createTempFile ()

    // fully declare the temporary file parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files.
    // The default filesystem does
    val tmpFile2: Path = Path.createTempFile (prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.default)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp file
    implicit val fs = FileSystem.default
    // fs will now be used by createTempFile
    val tmpFile3: Path = Path.createTempFile ()

    // a file system can also be used to create temporary files/directories
    fs.createTempFile ()
  }

  { // create temporary directories
    // Note: Both createTempFile and createTempDirectory have the same parameters
    import scalax.io.{Path,FileSystem}

    // by default the filesystem is the defaultFileSystem (surprise :-) )
    // using the default parameters will create a randomly named directory in
    // the system temp directory which will be deleted when the JVM exists
    val tmpFile1: Path = Path.createTempDirectory ()

    // fully declare the temporary directory parameters
    // all parameters have defaults so there are many option
    // Note that not all filesystems support creating temporary
    // files/directories.
    // The default filesystem does
    val tmpFile2: Path = Path.createTempDirectory (prefix="tmpFile",
                                          suffix="tmp",
                                          dir="/tmp",
                                          deleteOnExit=false)(FileSystem.default)

    // Using the same pattern as Path you can can use implicits
    // to declare the FileSystem that is used by make temp directory
    implicit val fs = FileSystem.default
    // fs will now be used by createTempDirectory
    val tmpFile3: Path = Path.createTempDirectory ()

    // a file system can also be used to create temporary files/directories
    fs.createTempFile ()
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
      case NonExistent (path) => println ("It does not exist!"+path)
      case _ => println ("I give up")
    }

    // Now match based on the permissions of the path
    // Set up matchers we want to use
    import Path.AccessModes._
    val RWE = new AccessMatcher (Read, Write, Execute)
    val RW = new AccessMatcher (Read, Write)
    val R = new AccessMatcher (Read)
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
    val InBinDir: PathMatcher = FileSystem.default.matcher ("/bin/*")

    // you can explicitly declare the GLOB matcher
    import PathMatcher.StandardSyntax.GLOB
    val StartsWithH: PathMatcher = FileSystem.default.matcher ("**/H*", GLOB)

    // a Regex matcher is also available
    import PathMatcher.StandardSyntax.REGEX
    val ContainsVowel: PathMatcher = FileSystem.default.matcher (".*[aeiou].*", REGEX)

    // If a filesystem supports a filesystem specific sytax you can declare that
    val CustomSyntaxMatcher: PathMatcher = FileSystem.default.matcher ("/tmp/@123", "customSyntax")

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

    // if path is a directory then you can use the \
    // methods to make a new path based on that directory
    val child1: Path = path \ "childFile"
    val child2: Path = path \ "dir1/f2"
    val child3: Path = path \ "dir1" \ "f3"
    val child4: Path = path \ Path ("f4")
    val child5: Path = path \ Path ("dir2") \ Path ("f5")

    // the resolve methods is essentially an alias for \ for those
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
    val readWrite: Boolean = path.checkAccess (Read, Write)

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
                 replace=true,
                 atomicMove=true)
  }


  { // search the contents of a directory and perform operations on the objects encountered

    // This set of examples use the contents method with the partial function parameter
    // there is another way of inspecting directory contents I another example

    import scalax.io.{Path, PathMatcher, DirectoryStream}
    import scalax.io.Path.Matching._

    val path:Path = Path("/tmp/")

    // print the name of each object in the directory
    path.children ().collect {case path => println (path.name)}

    // Now print names of each directory
    path.children ().collect {case File(file) => println (file.name)}

    // remove spaces from names of paths
    // renaming with this method can be dangerous because the stream may be calculated lazily on some filesystems and the renamed file could also be processed resulting in a infinite loop
    val ContainsSpace:PathMatcher = path.matcher ("* *")
    path.children ().collect {case ContainsSpace (path) => path.moveTo (Path (path.name.filter (_ != ' ')))}

    // count the number of directories
    val fileCount: Int = path.children ().collect{case File (f)=> f}.foldLeft (0){(count, _) => count+1}

    // A directory stream can also be constructed with a filter
    // this is sometime preferable because using a PathMatcher as a filter may offer operating system
    // native support for filtering
    // obviously useful when processing directories with many file (millions perhaps)
    // the filter is a function returning a PathMatcher because it is possible to define a
    // directoryStream that traverses many levels of the filesystem tree and the filter
    // function allows a new Matcher to be defined at each level of the tree
    val matcher: PathMatcher = path.matcher("S*")
    path.children (matcher).foreach (println _)

    path.children({_.isFile}).foreach (println _)
    
/*
 * Disabled until Java 7 version because implementation is impossible until then
    // Also you can attempt to perform atomic operations on a DirectoryStream
    // Since not all filesystems support atomic operations (Non in the pre java 7 implementation)
    // a check must be made to see if a secure directory stream was obtained
    path.secureDirectoryStream () match  {
      case Some(stream)  => stream.foreach (_.path.delete)
      case None => throw new AssertionError ("This filesystem does not support SecureDirectoryStream!")
    }
*/
  }

  { // Walk the directory tree

    import scalax.io.{Path, PathMatcher, DirectoryStream}

    val path:Path = Path("/tmp/")

    // by default only the files contained in the current path are returned but if depth
    // is set (<0 will traverse entire tree) then the stream will visit subdirectories in
    // pre-order traversal

    // search for a .gitignore file down to a depth of 4
    val gitIgnoreRestrictedTree: Option[Path] = path.descendants (depth=4).find (_.name == ".gitignore")

    // search for a .gitignore in the entire subtree
    val gitIgnoreFullTree: Option[Path] = path.descendants ().find (_.name == ".gitignore")

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

/*
 * Disabled until the Java 7 version because it can't be implemented until then
    path.secureTree (filters,  10 ) match {
      case Some(stream)  => stream.foreach (e => println (e.path))
      case None => throw new AssertionError ("This filesystem does not support SecureDirectoryStream!")
    }
*/
  }

  { // since the underlying filesystem could change to safely use the DirectoryStream API it is recommended to handle the
    // NotDirectoryException
    import scalax.io.{Path, NotDirectoryException, DirectoryStream}
    import scala.util.control.Exception._

    catching (classOf[NotDirectoryException]) opt {
      Path ("/tmp/dir").children() map ( _.name)
    } match {
      case None => println ("Not a direcory")
      case Some(names) => println ("files names = "+names)
    }
  }

  // the following several examples illustrate how to read and write files
  // One thing you will see in most examples is the assumption that the 
  // underlying file either is a file or does not exist. If the underlying 
  // file object is not a file at then a NotFileException  is thrown when 
  // a file method is invoked.there is one example that demonstrates the 
  // recommended way to write safe file code

  { // implement custom copy method
    import scalax.io.Path
    import scalax.io.OpenOption._

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    // not necessarily the most efficient way to copy but demonstrates use of reading/writing bytes
    Path("to").write(
      Path("from").bytes
    )

    // we could also append to file
    Path("to").write(
      Path("from").bytes)
  }

  { // read comma seperated file
    import scalax.io.Path

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val records: Traversable[Array[String]] = Path ("csv").lines().map (_ split 'x')
  }

  { // add all bytes in file together
    import scalax.io.{FileOps, Path}

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file:FileOps = Path("file")
    val sum: Int = file.bytesAsInts.reduceLeft (_ + _)
  }

  { // quickly an unsafely load file into memory

    // first load as strings and remove vowels
    import scalax.io.{FileOps, Path}

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file:FileOps = Path("file")
    val consonants = file.slurpString.filterNot (c => "aeiou" contains c)

    // ok now as bytes
    val (small, large) = file.byteArray partition (_ < 128)
  }

  { // iterate over all character in file
    import scalax.io.{FileOps, Path}
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path("file")
    val doubled: Traversable[String] = for ( c <- file.chars ) yield "" + c + c
  }

  { // read and print out all lines in a file
    import scalax.io.{FileOps, Path, Line}
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path("file")

    // by default the line terminator is stripped and is
    // auto detected
    file.lines() foreach println _

    // now do not strip terminator
    file.lines (includeTerminator = true) foreach print _

    // now declare explicitly the terminator
    // terminator is restricted to 1 or 2 characters
    file.lines (terminator = Line.Terminators.NewLine) foreach println _
  }

  { // explicitly declare the codecs to use
    import scalax.io.{FileOps, Path, Codec}
    val file: FileOps =  Path("file")

    // All methods for reading and writing characters/strings
    // have a codec parameter that used to explicitly declare the
    // codec to use
    file.chars (codec = Codec.ISO8859)

    // If there is not a constant for the desired codec
    // one can easily be created
    file.lines()(codec = Codec("UTF-16"))
  }

  { // recommended way to read and write a file
    import scalax.io.{
      FileOps, Path, NotFileException}
    import java.io.FileNotFoundException
    import scala.util.control.Exception._
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path("file")
    val result:Option[String] = catching (classOf[NotFileException],
                                          classOf[FileNotFoundException]) opt { file.slurpString}

    result match {
      case None => println("oops not a file maybe a directory?")
      case Some(data) => println (data)
    }
  }

  { // several examples of writing data
    import scalax.io.{
      FileOps, Path, Codec, OpenOption, Line}
    import OpenOption._
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8


    val file: FileOps =  Path ("file")

    // write bytes
    // By default the file write will replace
    // an existing file with the new data
    file.write (Array (1,2,3) map ( _.toByte))

    // another option for write is openOptions which allows the caller
    // to specify in detail how the write should take place
    // the openOptions parameter takes a collections of OpenOptions objects
    // which are filesystem specific in general but the standard options
    // are defined in the OpenOption object
    // in addition to the definition common collections are also defined
    // WriteAppend for example is a List(Create, Append, Write)
    file.write (List (1,2,3) map (_.toByte))

    // write a string to the file
    file.writeString("Hello my dear file")

    // with all options (these are the default options explicitely declared)
    file.writeString("Hello my dear file")(codec = Codec.UTF8)

    // Convert several strings to the file
    // same options apply as for writeString
    file.writeStrings( "It costs" :: "one" :: "dollar" :: Nil)

    // Now all options
    file.writeStrings("It costs" :: "one" :: "dollar" :: Nil,
                    separator="||\n||")(codec = Codec.UTF8)
  }

  { // perform an actions within a file lock
    import scalax.io.{FileOps, Path}

    val file: FileOps =  Path ("file")

    implicit val codec = scalax.io.Codec.UTF8

    // By default the entire file is locked with exclusive access
    val result: Option[String] = file.withLock() { s => 
      s.slurpString
    }

    // if the filesystem does not support locking then None will be returned
    result match {
      case None => file.slurpString // oh well this is the best I can do
      case Some(data) => data
    }

    def fail: Nothing = throw new AssertionError("Uh oh")

    // or perhaps we only lock part of the file
    val result2: Traversable[Byte] = file.withLock(10, 20) { s =>
      s.bytes slice (10,20)
    } getOrElse {fail}



  }

  { // demonstrate several ways to interoperate existing java APIs
    import scalax.io.{FileOps, Path}
    import java.io._
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    // some APIs require a stream or channel. Using one of the io resources you can safely call the method and be guaranteed that the stream will be correctly closed and exceptions handled
    // see the documentation in resource.ManagedResource for details on all the options available
    def javaApiEntryPoint(stream: InputStream) = {
      // do something interesting
      stream.read()
    }

    // here is the code for calling that method
    file.inputStream.acquireFor (javaApiEntryPoint)

    // other APIs use inversion of to obtain the io object.
    // here is how to get a raw java OutputStream from a file
    // and just for good measure it will be a BufferedOutputStream
    // streams and writer both have buffered versions, similar to their java counterparts
    val out: OutputStream = file.outputStream().buffered.open
  }

  { // several examples of obtaining Resources
    import scalax.io._
    import OpenOption._
    import java.nio.channels._
    import scalax.io.resource.{
      Resource, Bufferable, InputStreamResource,
      OutputStreamResource, ByteChannelResource,
      ReaderResource, WriterResource
    }
    import java.io.{
        FileInputStream,
        InputStream, BufferedInputStream,
        OutputStream, BufferedOutputStream,
        Reader, BufferedReader,
        Writer, BufferedWriter
    }
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    // get various input streams, readers an channels
    val in: InputStreamResource[InputStream] = file.inputStream
    val bufferedIn: InputStreamResource[BufferedInputStream] = in.buffered
    val readableChannel: Resource[ReadableByteChannel] = in.readableByteChannel
    val reader: ReaderResource[Reader] = in.reader
    val bufferedReader: ReaderResource[BufferedReader] = reader.buffered

    // get various output streams and channels
    // get default OutputStream
    // default will create fileif it does not exist and overwrite if it does
    var out: OutputStreamResource[OutputStream] = file.outputStream()
    // create a appending stream
    var out2: OutputStreamResource[OutputStream] = file.outputStream (WriteAppend:_*)
    val bufferedOut: OutputStreamResource[BufferedOutputStream] = out.buffered
    val writableChannel: Resource[WritableByteChannel] = out.writableByteChannel
    val writer: WriterResource[Writer] = out.writer
    val bufferedWriter: WriterResource[BufferedWriter] = writer.buffered
    // TODO copy examples from input section

    // examples getting ByteChannels
    // default is a read/write/create channel
    val channel: ByteChannelResource[ByteChannel] = file.channel()
    val channel2: ByteChannelResource[ByteChannel] = file.channel(Read,Write,Append)

    // Not all filesystems can support FileChannels so the fileChannel method returns an option
    file.fileChannel() foreach { fc => println("got a file channel") }
  }

  { // examples of patching a file
    import scalax.io.{FileOps, Path, Codec}
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    // write "people" at byte 6
    // if the file is < 6 bytes an underflow exception is thrown
    // if the patch extends past the end of the file then the file is extended
    file.patchString(6, "people")
    file.patchString(6, "people")(Codec.UTF8)

    // patch the file with a traversable of bytes
    file.patch(6, "people".getBytes)
  }

  { // when several operation need to be performed on a file it is often more performant to perform them within an function passed to the open method
    // this is because the underlying filesystem has options for optimizing the use of the file channels
    // for example a file could be mapped into memory for the duration of the function and all operations could be performed using the same channel object
    import scalax.io.{FileOps, Path, Codec}
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    file.open()( f => {
      val s = f.slurpString
      file.writeString(s.replaceAll("l", "L"))
    })
  }

  { // Examples of non-file IO
    import scalax.io._
    import scalax.io.resource.Resource
    import resource.ManagedResource
    import java.net.URL
    import java.io.{
      ObjectInputStream, InputStreamReader, ByteArrayOutputStream,
      PrintStream, OutputStreamWriter, BufferedReader
    }
    
    implicit val defaultCodec: Codec = scalax.io.Codec.UTF8

    // Note that in these example streams are closed automatically
    // Also note that normally a constructed stream is not passed to factory method because most factory methods are by-name parameters (=> R)
    // this means that the objects here can be reused without worrying about the stream being previously emptied
    val url = new URL("www.scala-lang.org")
    // A ReadChars (a trait of FileOps) object can be created from a stream or channel
    Resource.fromInputStream(url.openStream()).reader.lines() foreach println _
    Resource.fromReader(new InputStreamReader(url.openStream())).lines() foreach println _
    // ReadBytes can also be constructed
    val bytes: Traversable[Byte] = Resource.fromInputStream(url.openStream()).bytes
    Path("scala.html") write bytes

    // WriteChars and WriteBytes can be used to simplify writing to OutputStreams
    Resource.fromOutputStream(new ByteArrayOutputStream()).writer.writeString("howdy")
    Resource.fromWriter(new OutputStreamWriter(new ByteArrayOutputStream())).writeString("howdy")
    Resource.fromOutputStream(new PrintStream(new ByteArrayOutputStream())).writer.writeString("howdy")

    // Channels and streams can also be wrapped in Resource objects
    val resource = Resource.fromInputStream (url.openStream ())
    resource.buffered acquireFor {in => println (in.read())}

    // Resources have convenience methods for converting between common types of resources
    resource.reader.buffered  acquireFor {in => println (in.asInstanceOf[BufferedReader].readLine())}

    // a more general way of converting between resources is to use the ManagedResource API
//    val objectIn: ManagedResource[ObjectInputStream] = resource map (s => new ObjectInputStream (s))
  }

}
