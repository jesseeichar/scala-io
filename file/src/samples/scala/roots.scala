import scalax.file.defaultfs.DefaultPath

/**
 * Look up the rots of a path
 */
object Roots {
  def listRoots {
    // list roots of defaultFileSystem
    import scalax.file.{Path, FileSystem}
    val roots1: Set[DefaultPath] = Path.roots
    // This method delegates to the defaultFileSystem as follows
    val roots2: Set[DefaultPath] = FileSystem.default.roots
  }
}
