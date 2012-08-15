package scalax.file

sealed trait AccessMode
/**
 * Enumeration of the Access modes possible for accessing files
 */
object AccessModes {
  case object Execute extends AccessMode
  case object Read extends AccessMode
  case object Write extends AccessMode
  def values: Set[AccessMode] = Set(Execute, Read, Write)
}