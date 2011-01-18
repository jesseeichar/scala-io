object PathMatcherSamples {
  /**
   * Match a Path against the full path as a string
   */
  def matchFullPathAsString {
    import scalax.file.Path
    Path("/tmp/file") match {
      case Path("/tmp/file") => println("it's a match")
      case _ => println("no match")
    }
    Path("/tmp/file") match {
      case Path(stringPath) => println("path as a string is:" + stringPath)
      case _ => println("no match")
    }
  }

  /**
   * demonstrate matching using the matchers that are provided in Path.Matching
   */
  def defaultMatchers {
    import scalax.file.Path
    import scalax.file.PathMatcher._

    // This example tests if the path is a file, directory, exists or does not exist
    Path("/tmp/file") match {
      case IsFile(file) => println("it's a file!" + file)
      case IsDirectory(dir) => println("it's a directory!" + dir)
      case Exists(path) => println("It exists... but what is it?" + path)
      case NonExistent(path) => println("It does not exist!" + path)
      case _ => println("I give up")
    }

    // Now match based on the permissions of the path
    // Set up matchers we want to use
    import Path.AccessModes._
    val RWE = new AccessMatcher(Read, Write, Execute)
    val RW = new AccessMatcher(Read, Write)
    val R = new AccessMatcher(Read)
    Path("/tmp/file") match {
      case RWE(path) => println("path is rwe" + path)
      case RW(path) => println("path is rw" + path)
      case R(path) => println("path is r" + path)
    }
  }

  def fileSystemSupportedMatchers {

    import scalax.file.{Path, PathMatcher, FileSystem}

    // there are three factory methods that matchers
    // Path.matcher (instance method)
    // FileSystem.matcher

    // default type of matcher created is a glob matcher
    val InTmpDir: PathMatcher = Path("/tmp/file").matcher("/tmp/**")

    // If you can also create through the FileSystem
    val InBinDir: PathMatcher = FileSystem.default.matcher("/bin/*")

    // you can explicitly declare the GLOB matcher
    import PathMatcher.StandardSyntax.GLOB
    val StartsWithH: PathMatcher = FileSystem.default.matcher("**/H*", GLOB)

    // a Regex matcher is also available
    import PathMatcher.StandardSyntax.REGEX
    val ContainsVowel: PathMatcher = FileSystem.default.matcher(".*[aeiou].*", REGEX)

    // If a filesystem supports a filesystem specific sytax you can declare that
    val CustomSyntaxMatcher: PathMatcher = FileSystem.default.matcher("/tmp/@123", "customSyntax")

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
