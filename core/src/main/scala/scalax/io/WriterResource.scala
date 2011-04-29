package scalax.io

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    opener: => A,
    closeAction:CloseAction[A])
  extends WriteCharsResource[A]
  with ResourceOps[A, WriterResource[A]]  {

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new WriterResource(opener,closeAction +: newAction)

  protected def writer = this
}
