package scalax.file.ramfs
package actor

private[ramfs] sealed trait RamFsMsg

private[ramfs] sealed class SyncRamFsMsg[Response] (cl: Class[Response]) extends RamFsMsg {
  def send(actor: RamFsActor) = actor !? this match {
    case rsp if rsp.getClass.isAssignableFrom(cl) => 
      rsp.asInstanceOf[Response]
    case rsp =>
      throw new IllegalArgumentException("Expected a response of type: "+cl+" but was "+rsp)
  }
}
object RamFsMsg {
	protected[ramfs] case object Stop extends RamFsMsg
	protected[ramfs] case object IsRunning extends SyncRamFsMsg(classOf[Boolean])
	protected[ramfs] case class CreateFile(path: RamPath, createParents: Boolean, attr: Map[RamAttributes.RamAttribute,Object]) extends RamFsMsg 
	protected[ramfs] case class CreateDir(path: RamPath, createParents: Boolean, attr: Map[RamAttributes.RamAttribute,Object]) extends RamFsMsg 
	protected[ramfs] case class Lookup(path: RamPath) extends SyncRamFsMsg(classOf[RamFsResponse.Lookup]) 
}
