object Roots { 
  // list roots of defaultFileSystem
  import scalax.io.{Path, FileSystem}
  val roots1: List[Path] = Path.roots
  // This method delegates to the defaultFileSystem as follows
  val roots2: List[Path] = FileSystem.default.roots
}
