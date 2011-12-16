
/**
 * PathSets is a DSL API for selecting collections of Paths to operate on.
 * <p>
 * Essentially it is a glob style DSL with operations for combining
 * sets
 * </p>
 */
object CreatingPathSets {

  /**
   * Demonstrate making PathSets and Combining Paths
   */
  def standardExamples {
    import scalax.file.Path
    import Path._
    import scalax.file.PathSet
    // find all .java files in a tmp or one of its sub-directories
    val descendantJavaFiles : PathSet[Path] = "/tmp" ** "*.java"

    // PathSet are lazily evaluated and if the underlying filesystem changes between
    // calls to the PathSet the PathSet will return different results
    // Calling toSet will return an unchanging set of files (but not guaranteed to
    val javaFileSet:Set[Path] = descendantJavaFiles.toSet

    // find all .scala files in src/sbt but not in subdirectories
    val childScalaFiles:PathSet[Path] = "src" \ "sbt" * "*.scala"

    val mainAndLib = ("src" \ "main") +++ "lib"

    // select all files in src/main, lib and in target/classes excluding everything ending in .txt
    val multiPath:PathSet[Path] = ("src" \ "main") +++ "lib" +++ ("target" \ "classes") --- "**/*.txt"

    // select all jar files in both lib and target
    val jars:PathSet[Path] = ("lib" +++ "target") * "*.jar"

    // A common problem is excluding version control directories. This can be accomplished as follows:
    val sources:PathSet[Path] = ("src" ** "*.scala") --- ("src" ** ".svn" ** "*.scala")

    val altsources = "src" ** "*.{scala,java}"
    def imageResources = "src"/"main"/ "resources" * "*.png" filterNot { _.name == "logo.png"}
  }

  /**
   * Using PathMatching to create path sets.  <p>Most examples compare using PathMatcher vs using a function.
   * Using a function is more flexible and more composeable but PathMatchers can often be passed to the underlying
   * operating system (in Java 7+) and therefore are usually more performant than the function based solution.</p>
   */
  def pathMatcherPathSets {
    import scalax.file.Path
    import Path._
    import scalax.file.PathSet
    import scalax.file.PathMatcher._
    

    // two ways to select all files
    val files = Path("tmp") ** IsFile
    val files2 = Path("tmp") ** ((_:Path).isFile)
    
    // select all directories
    val directories = Path("tmp") ** IsDirectory
    val directories2 = Path("tmp") ** ((_:Path).isDirectory)
    
    // find all writeable, executable files
    import Path.AccessModes._
    val writeable = Path("tmp") ** AccessMatcher(Write, Execute) // can put multiple 
    import scalax.file.attributes._
    val writeable2 = Path("tmp") ** AttributeMatcher(WriteAccessAttribute(true), ExecuteAccessAttribute(true))
    val writeable3 = Path("tmp") ** ((_:Path).canWrite)
    
    // find all files with a particular datestamp that are writeable but not executable
    import Path.AccessModes._
    val timeStamped = Path("tmp") ** AttributeMatcher(WriteAccessAttribute(true), ExecuteAccessAttribute(false), LastModifiedAttribute(123456000L))
    val timeStamped2 = Path("tmp") ** ((p:Path) => p.canWrite && p.canExecute && p.lastModified == 123456000L)
  }
}
