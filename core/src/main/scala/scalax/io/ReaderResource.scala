package scalax.io

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    opener: => A,
    closeAction:CloseAction[A])
  extends BufferableReadCharsResource[A, BufferedReader]
  with ResourceOps[A, ReaderResource[A]] {

  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,closeAction +: newAction)

  def buffered : BufferedReaderResource[BufferedReader] = {
    def nResource = {
      val a = open()
      new BufferedReader(a) with ResourceAdapting.Adapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromBufferedReader(nResource).appendCloseAction(closer)
  }

  override def chars : ResourceView[Char]= ResourceTraversable.readerBased(this).view
}
