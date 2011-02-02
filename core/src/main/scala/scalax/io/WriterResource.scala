package scalax.io

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
class WriterResource[+A <: Writer](opener: => A, closeAction:CloseAction[A]) extends BufferableWriteCharsResource[A, BufferedWriter]
    with ResourceOps[A, WriterResource[A]]  {
  def open() = opener

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def buffered : WriterResource[BufferedWriter] = {
    def nResource = {
      val a = open()
      new BufferedWriter(a) with ResourceAdapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromBufferedWriter(nResource)(closer)
  }
  protected def writer = this
}
