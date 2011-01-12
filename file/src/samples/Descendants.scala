import scalax.io.{Path}

object Descendants {

  val path:Path = Path("/tmp/")

  // Walk the directory tree
  def `descendant processing` {

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
    // relativePath is the path relative from origin to the path that will be contained in the PathSet
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

}
