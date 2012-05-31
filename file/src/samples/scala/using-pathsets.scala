/**
 * Search the contents of a directory and perform operations on the objects encountered
 */
object UsingPathSets {

  /**
   * Remove spaces from names of paths
   * renaming with this method can be dangerous because
   * the stream may be calculated lazily on some filesystems
   * and the renamed file could also be processed resulting
   * in a infinite loop
   */
  def removeSpaces {
    import scalax.file.{ Path, PathMatcher }
    import scalax.file.PathMatcher._

    val path: Path = Path("/tmp/")
    val ContainsSpace: PathMatcher[Path] = path.matcher("* *")
    path.children(ContainsSpace).foreach { path => path.moveTo(Path(path.name.filter(_ != ' '))) }
  }

  /*
     * Count the number of directories
     */
  def countDirectories {
    import scalax.file.{ Path, PathMatcher }
    import scalax.file.PathMatcher._

    val path: Path = Path("/tmp/")
    val fileCount: Int = path.children(IsFile).size
  }

  /*
     * A PathSet can also be constructed with a filter
     * this is sometime preferable because using a PathMatcher as a filter may offer operating system
     * native support for filtering
     * obviously useful when processing directories with many file (millions perhaps)
     * the filter is a function returning a PathMatcher because it is possible to define a
     * directoryStream that traverses many levels of the filesystem tree and the filter
     * function allows a new Matcher to be defined at each level of the tree
     */
  def filterContents {
    import scalax.file.{ Path, PathMatcher }
    import scalax.file.PathMatcher._

    val path: Path = Path("/tmp/")
    val matcher: PathMatcher[Path] = path.matcher("S*")
    path.children(matcher).foreach(println _)

    path.children(IsFile).foreach(println _)
  }

  /**
   * All operations/filters that can be also performed on all descendants
   * of a path.  Calling the descendants method instead of children will
   * visit all descendants of the Path rather than just the children.
   */
  def descendantProcessing {
    import scalax.file.Path

    val path: Path = Path("/tmp/")

    // by default only the files contained in the current path are returned but if depth
    // is set (<0 will traverse entire tree) then the stream will visit subdirectories in
    // pre-order traversal

    // search for a .gitignore file down to a depth of 4
    val gitIgnoreRestrictedTree: Option[Path] = path.descendants(depth = 4).find(_.name == ".gitignore")

    // search for a .gitignore in the entire subtree
    val gitIgnoreFullTree: Option[Path] = path.descendants().find(_.name == ".gitignore")

    // A (slightly) more efficient version.
    val gitIgnoreFullTreeUsingFilter: Option[Path] = path.descendants((_:Path).name == ".gitignore").headOption
  }

  /**
   * See {creating-pathsets} for more details on creating PathSets.
   * <p>
   * This examples selects all descendants of src/main that
   * are scala files and starts with an s.
   * </p>
   */
  def descendantsUsingPathSets {
    import scalax.file.Path

    val path: Path = Path("/tmp/")

    path / "src" / "main" ** "s*.scala" foreach (println)

  }

  /**
   * Operate on all children
   */
  def children {
    import scalax.file.{Path, PathMatcher }
    import scalax.file.PathMatcher._

    val path: Path = Path("/tmp/")

    // print name of all files and directories that are children of path
    path.children().foreach { path => println(path.name) }

    // Print only the files
    path.children().filter(IsFile).foreach(path => println(path.name))
  }

  def laziness {
    import scalax.file.{Path, PathMatcher }
    import scalax.file.PathMatcher._

    val path: Path = Path("/tmp/")

    // all children of path.  the filesystem is not read touched
    val allChildren = path.children()

    // all files of children.  Still no filesystem access
    val files = allChildren.filter(IsFile)

    // all files smaller than 1000 bytes.  Still no filesystem access
    val smallFiles = files.filter(_.size.forall(_ < 1000))

    // sizes of files.  still not filesystem access
    val sizes = smallFiles.flatMap(_.size)

    //val actualSizes = sizes.force

  }
  /*
 * Disabled until Java 7 version because implementation is impossible until then
    // Also you can attempt to perform atomic operations on a PathSet
    // Since not all filesystems support atomic operations (Non in the pre java 7 implementation)
    // a check must be made to see if a secure directory stream was obtained
    path.secureDirectoryStream () match  {
      case Some(stream)  => stream.foreach (_.path.delete)
      case None => throw new AssertionError ("This filesystem does not support SecureDirectoryStream!")
    }
*/
}
