/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scalax.io

/**
 * Object containing several objects that make matching
 * certain types of Paths much easier.
 * <p>
 * Example:
 * <pre><code>
 * val Apple = path.matcher (pattern="[aA]pple", syntax="glob")
 * val ReadWrite = new AccessMatcher (Read, Write)
 * path match {
 *   case File(f) => println("A file was found")
 *   case Directory(d) => println("A directory was found")
 *   case NonExistent(e) => println("Path does not Exist")
 *   case Apple(apple) => println("A path named apple was found")
 *   case ReadWrite(apply) => println("Path is readable and writeable")
 *   case Path("c:/dir/file") => println("Found a path that is c:/dir/file or
                                          c:\\dir\\file")
 * }
 * </code></pre>
 * Note: The Apple portion of the example is not part of Matching but
 *       a critical part of matching Paths and thus is included in
 *       this example
 */
object Matching {
  /** matches a path if it is a file (and exists)*/
  object File {
    def unapply(path:Path):Option[Path] = Some(path).filter {_.isFile}
  }
  /** matches a path if it is a Directory (and exists)*/
  object Directory {
    def unapply(path:Path):Option[Path] = Some(path).filter {_.isDirectory}
  }
  /** matches a path if it is Exists */
  object Exists {
    def unapply(path:Path):Option[Path] = Some(path).filter {_.exists}
  }
  /** matches a path if it does not Exist */
  object NonExistent {
    def unapply(path:Path):Option[Path] = Some(path).filter {_.notExists}
  }
  /**
   * Matches a path if the access modes are applicable
   * for the file.
   * <p>
   * If the file does not exist this matcher will not match
   * </p>
   *
   * @param accessModes
   *          the access modes that must be applicable
   *          on the path object.
   */
  class AccessMatcher (accessModes: Path.AccessModes.AccessMode*) {
    val accessModeSet = Set(accessModes:_*)
    def unapply(path: Path): Option[Path] = {
      if(accessModeSet.intersect(path.access).size == accessModeSet.size) Some(path)
      else None
    }
  }
  object AccessMatcher {
    def apply(accessModes: Path.AccessModes.AccessMode*) = new AccessMatcher(accessModes:_*)
  }
}