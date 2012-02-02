package scalax.io
package unmanaged

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    resource: A,
    closeAction:CloseAction[A] = CloseAction.Noop,
    descName:ResourceDescName = UnknownName())
  extends ReadCharsResource[A]
  with ResourceOps[A, ReaderResource[A]]
  with UnmanagedResource {
  
  self =>
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
  override def unmanaged = this

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open)

  override def toString: String = "ReaderResource("+descName.name+")"
}
