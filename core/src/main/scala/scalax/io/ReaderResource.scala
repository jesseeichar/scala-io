package scalax.io

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
class ReaderResource[+A <: Reader](opener: => A, closeAction:CloseAction[A]) extends BufferableReadCharsResource[A, BufferedReader]
    with ResourceOps[A, ReaderResource[A]] {
  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,closeAction +: newAction)

  def buffered : ReaderResource[BufferedReader] = {
    def nResource = {
      val a = open()
      new BufferedReader(a) with ResourceAdapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromBufferedReader(nResource)(closer)
  }

  override def chars : ResourceView[Char]= ResourceTraversable.readerBased(this).view
}
