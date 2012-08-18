package scalax.file.ramfs;

import java.nio.file.FileStore
import java.nio.file.attribute._

class RamFileStore(id: RamFsId) extends FileStore {
  val supportedViewClasses = Seq(classOf[BasicFileAttributeView])
  val supportedViewNames = Seq("basic")
  override val name = "RamFS(%s)".format(id.id);
  override val `type` = "ramfs"
  override val isReadOnly = false

  override def getTotalSpace(): Long = Runtime.getRuntime().maxMemory()
  override def getUsableSpace() = Runtime.getRuntime().freeMemory()
  override def getUnallocatedSpace() = Runtime.getRuntime().freeMemory()

  override def supportsFileAttributeView(typeOf: Class[_ <: FileAttributeView]): Boolean = supportedViewClasses contains typeOf
  override def supportsFileAttributeView(name: String): Boolean = supportedViewNames contains name
  
  override def getFileStoreAttributeView[V <: FileStoreAttributeView](typeOf: Class[V]): V = null.asInstanceOf[V]
  override def getAttribute(attribute: String): Object = null
}
