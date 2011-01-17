import scalax.file.{Path, PathMatcher}
import scalax.file.PathMatcher._

/**
 * search the contents of a directory and perform operations on the objects encountered
 */
object Children {
    val path:Path = Path("/tmp/")

    /**
     * Operate on all children
     */
    def children {
      // print the name of each object in the directory
      path.children ().collect {case path => println (path.name)}

      // Now print names of each directory
      path.children ().collect {case IsFile(file) => println (file.name)}
    }

    /**
     * Remove spaces from names of paths
     * renaming with this method can be dangerous because
     * the stream may be calculated lazily on some filesystems
     * and the renamed file could also be processed resulting
     * in a infinite loop
     */
    def `remove spaces` {
      val ContainsSpace:PathMatcher = path.matcher ("* *")
      path.children ().collect {case ContainsSpace (path) => path.moveTo (Path (path.name.filter (_ != ' ')))}
    }

    /*
     * Count the number of directories
     */
    def `count directories` {
      val fileCount: Int = path.children ().collect{case IsFile (f)=> f}.foldLeft (0){(count, _) => count+1}
    }

    /*
     * A directory stream can also be constructed with a filter
     * this is sometime preferable because using a PathMatcher as a filter may offer operating system
     * native support for filtering
     * obviously useful when processing directories with many file (millions perhaps)
     * the filter is a function returning a PathMatcher because it is possible to define a
     * directoryStream that traverses many levels of the filesystem tree and the filter
     * function allows a new Matcher to be defined at each level of the tree
     */
    def `filter contents` {
      val matcher: PathMatcher = path.matcher("S*")
      path.children (matcher).foreach (println _)

      path.children(IsFile).foreach (println _)
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
