package scalax.io
package unmanaged

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    resource: A,
    val context: ResourceContext[A])
  extends ReadCharsResource[A]
  with ResourceOps[A, ReadCharsResource[A], ReaderResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource,context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open)

  override def toString: String = "ReaderResource("+context.descName.name+")"
}
