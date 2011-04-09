package scalax.io

import java.io._

/**
 * A ManagedResource for accessing and using BufferedWriters.  Class can be created using the [[scalax.io.Resource]] object.
 */
class BufferedWriterResource[+A <: BufferedWriter] (opener: => A, closeAction:CloseAction[A]) extends WriterResource[A](opener,closeAction)
    with ResourceOps[A, BufferedWriterResource[A]]  {
  override def prependCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedWriterResource(opener,newAction :+ closeAction)
  override def appendCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedWriterResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  override def buffered : BufferedWriterResource[BufferedWriter] = this
}

/**
 * A ManagedResource for accessing and using BufferedReader.  Class can be created using the [[scalax.io.Resource]] object.
 */
class BufferedReaderResource[+A <: BufferedReader] (opener: => A, closeAction:CloseAction[A]) extends ReaderResource[A](opener,closeAction)
    with ResourceOps[A, BufferedReaderResource[A]]  {
  override def prependCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedReaderResource(opener,newAction :+ closeAction)
  override def appendCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedReaderResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  override def buffered : BufferedReaderResource[BufferedReader] = this
}

/**
 * A ManagedResource for accessing and using BufferedInputStream.  Class can be created using the [[scalax.io.Resource]] object.
 */
class BufferedInputStreamResource[+A <: BufferedInputStream] (opener: => A, closeAction:CloseAction[A]) extends InputStreamResource[A](opener,closeAction,() => None)
    with ResourceOps[A, BufferedInputStreamResource[A]]  {
  override def prependCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedInputStreamResource(opener,newAction :+ closeAction)
  override def appendCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedInputStreamResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  override def buffered : BufferedInputStreamResource[BufferedInputStream] = this
}

/**
 * A ManagedResource for accessing and using BufferedOutputStream.  Class can be created using the [[scalax.io.Resource]] object.
 */
class BufferedOutputStreamResource[+A <: BufferedOutputStream] (opener: => A, closeAction:CloseAction[A]) extends OutputStreamResource[A](opener,closeAction)
    with ResourceOps[A, BufferedOutputStreamResource[A]]  {
  override def prependCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedOutputStreamResource(opener,newAction :+ closeAction)
  override def appendCloseAction[B >: A](newAction: CloseAction[B]) = new BufferedOutputStreamResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  override def buffered : BufferedOutputStreamResource[BufferedOutputStream] = this
}
