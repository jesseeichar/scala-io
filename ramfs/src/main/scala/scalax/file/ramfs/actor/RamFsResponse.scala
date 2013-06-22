package scalax.file.ramfs
package actor

/**
 * Represents a 
 */
protected[ramfs] sealed trait RamFsResponse
object RamFsResponse {
	protected[ramfs] case class Lookup(node: Option[NodeRef]) extends RamFsResponse
}
