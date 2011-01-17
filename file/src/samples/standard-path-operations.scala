/**
 * Common/simple Path operations
 * Nothing too fancy here. Coolest is the resolving child
 * files and directories
 */
object StdPathOps {
    import scalax.file.Path
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
    // path object.  One is similar to the java.file.File.  The other is based
    // a single query to test several attributes at once.

    // first the java.file.File way
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
    val notExists: Boolean = path.nonExistent

    val hidden: Boolean = path.isHidden
    val isSymLink: Boolean = path.isSymlink

    // query last modified information
    val lastModified: Long = path.lastModified
    path.lastModified = System.currentTimeMillis

    val length = path.size

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
}
