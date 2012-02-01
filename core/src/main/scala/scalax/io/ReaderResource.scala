package scalax.io

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    opener: => A,
    closeAction:CloseAction[A] = CloseAction.Noop,
    descName:ResourceDescName = UnknownName())
  extends ReadCharsResource[A]
  with ResourceOps[A, ReaderResource[A]] {
  
  self =>
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new ReaderResource[A](opener, CloseAction.Noop, descName) with UnmanagedResource {
    private[this] val resource = self.open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open)

  override def toString: String = "ReaderResource("+descName.name+")"
}
