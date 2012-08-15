
/**
 * PathSets is a DSL API for selecting collections of Paths to operate on.
 * <p>
 * Essentially it is a glob style DSL with operations for combining
 * sets
 * </p>
 */
import scalax.file.FileAttributeImpl

object CreatingPathSets {

  /**
   * Demonstrate making PathSets and Combining Paths
   */
  def standardExamples {
    import scalax.file.Path
    import scalax.file.ImplicitConversions._
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
    import scalax.file.FileAttributeImpl
    import Path._
    import scalax.file.PathSet
    import scalax.file.PathMatcher._
    import java.nio.file.attribute.PosixFilePermissions

    // two ways to select all files
    val files = Path("tmp") ** IsFile
    val files2 = Path("tmp") ** ((_:Path).isFile)

    // select all directories
    val directories = Path("tmp") ** IsDirectory
    val directories2 = Path("tmp") ** ((_:Path).isDirectory)

    // find all writeable, executable files
    import scalax.file.AccessModes._
    val writeable = Path("tmp") ** AccessMatcher(Write, Execute) // can put multiple
    
    
    val writeable2 = Path("tmp") ** AttributeMatcher(FileAttributeImpl("read-only", true))
    val writeable3 = Path("tmp") ** ((_:Path).canWrite)

    // find all files with a particular datestamp that are writeable but not executable
    import scalax.file.AccessModes._
    val timeStamped = Path("tmp") ** AttributeMatcher(PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("_wx_wx---")), FileAttributeImpl("lastModified", 123456000L) )
    val timeStamped2 = Path("tmp") ** ((p:Path) => p.canWrite && p.canExecute && p.lastModified == 123456000L)
  }

  /**
   * Illustrate how to do matching using regular expression
   */
  def regexBasedPathSets {
    import scalax.file.Path

    // this will find all paths that are a child of "." that == eng or fra or deu
    // notice that the .r at the end, this converts the string to a regular expression
    // and so the PathFilter that is created using the regular expression to do the match
    val engORfraORdeu = Path(".") * "(eng)|(fra)|(deu)".r


    // in contract to the previous PathSet this consists of the one path which has the name (eng)|(fra)|(deu)
    // notice that the argument is NOT a regular expression so it is interpretted as a glob expression
    val engfradeu = Path(".") * "(eng)|(fra)|(deu)"

    // finds all strings.xml files in sub directories of eng fra or deu
    val strings = Path(".") * "(eng)|(fra)|(deu)".r ** "strings.xml"
  }

  def globBasedPathSets {
    import scalax.file.Path

    // All java files in any subdirectory of .
    val javaFiles = Path(".") ** "*.java"

    // All java or scala files in subdirectory of
    val jvmFiles = Path(".") ** "*.{java,scala}"

    // A crazy example that illustrates a substantial portion of the glob
    // syntax
    val complexSearch = Path(".") ** "a/?/[a-z]/d.{x,y}"
  }
}
