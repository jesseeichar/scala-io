package scalax.io

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    opener: => A,
    closeAction:CloseAction[A])
  extends ReadCharsResource[A]
  with ResourceOps[A, ReaderResource[A]] {

  def open() = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReaderResource(opener,closeAction +: newAction)

  override def chars : ResourceView[Char]= ResourceTraversable.readerBased(this).view
}
