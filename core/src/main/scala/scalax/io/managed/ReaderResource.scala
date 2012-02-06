package scalax.io
package managed

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    opener: => A,
    val context:ResourceContext[A])
  extends ReadCharsResource[A]
  with ResourceOps[A, ReadCharsResource[A], ReaderResource[A]] {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  def unmanaged = new scalax.io.unmanaged.ReaderResource[A](opener, context)

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open)

  override def toString: String = "ReaderResource("+context.descName.name+")"
}
