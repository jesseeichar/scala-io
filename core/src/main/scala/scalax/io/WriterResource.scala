package scalax.io

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] protected[io](opener: => A, closeAction:CloseAction[A]) extends BufferableWriteCharsResource[A, BufferedWriter]
    with ResourceOps[A, WriterResource[A]]  {
  def open() = opener

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def buffered : BufferedWriterResource[BufferedWriter] = {
    def nResource = {
      val a = open()
      new BufferedWriter(a) with ResourceAdapting.Adapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromBufferedWriter(nResource).appendCloseAction(closer)
  }
  protected def writer = this
}
