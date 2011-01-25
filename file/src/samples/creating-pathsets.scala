import scalax.file.Path
import Path._
import scalax.file.PathSet

/**
 * PathSets is a DSL API for selecting collections of Paths to operate on.
 * <p>
 * Essentially it is a glob style DSL with operations for combining
 * sets
 * </p>
 */
object PathSetSamples {

  /**
   * Demonstrate making PathSets and Combining Paths
   */
  def standardExamples {
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

    // In some situations, it is useful to define the directory a path is relative to.
    // For example, the package action in sbt packages compiled classes and all files under resources.
    // The full path name should not be used in the jar, however. This is where the ## operator comes
    // in. The paths for this situation would look like:

    // TODO coming soon
    //val allClasses:PathSet[Path] = ("target" / "classes" asBase) ** "*.class"
    //val allResources:PathSet[Path] = ("src" / "main" / "resources" asBase) ** "*"
    //val toPackage:PathSet[Path] = allClasses +++ allResources

    // A common problem is excluding version control directories. This can be accomplished as follows:
    val sources:PathSet[Path] = ("src" ** "*.scala") --- ("src" ** ".svn" ** "*.scala")

    val altsources = "src" ** "*.{scala,java}"
    def imageResources = "src"/"main"/"resources" * "*.png" filterNot { _.name == "logo.png"}
  }

}
