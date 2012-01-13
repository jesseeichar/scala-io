package scalax.io
package processing;

trait Processor[+A] {
  self =>
  
  def traversable[B](implicit transformer: ProcessorTransformer[B, A, LongTraversable[B]]): LongTraversable[B] = transformer.transform(this)

  def acquireFor[U](f: A => U): U = {
    val initialized = init
    try f(initialized.execute)
    finally initialized.cleanUp
  }

  private[processing] def init: Opened[A]
  def foreach[U](f: A => U): Unit = acquireFor(f)
  def map[U](f: A => U) = Processor[Opened[A], U](init, in => f(in.execute()), _.cleanUp)
  def flatMap[U](f: A => Processor[U]) = {
    Processor[(Opened[A], Opened[U]), U]({
      val currentOpenProcessor = self.init
      (currentOpenProcessor, f(currentOpenProcessor.execute()).init)
    },
      _._2.execute(),
      in => { in._2.cleanUp; in._1.cleanUp() })
  }
}
private[processing] trait Opened[+A] {
  def execute(): A
  def cleanUp(): Unit
}

object Processor {
  def apply[B, A](before: => B, valueFactory: B => A, after: B => Unit) = new Processor[A] {
    private[processing] def init = {
      val resource = before
      new Opened[A] {
        def execute() = valueFactory(resource)
        def cleanUp() = after(resource)
      }
    }
  }
  def apply[A](valueFactory: => A) = new Processor[A] {
    private[processing] def init = {
      new Opened[A] {
        def execute() = valueFactory
        def cleanUp() = ()
      }
    }
  }
  def empty[A] = new Processor[Nothing] {
    private[processing] def init = null.asInstanceOf[Opened[Nothing]]
    override def foreach[U](f: Nothing => U): Unit = ()
    override def map[U](f: Nothing => U) = this
    override def flatMap[U](f: Nothing => Processor[U]) = this
  }
}

private[io] class CloseableIteratorProcessor[+A](private[processing]iter: => CloseableIterator[A]) extends Processor[ProcessorAPI[A]] {
  private[processing] def init = {
    val iterInstance = iter
    val ProcessorAPI = new ProcessorAPI(iterInstance)
    new Opened[ProcessorAPI[A]] {
      def execute() = ProcessorAPI
      def cleanUp() = iterInstance.close()
    }
  }
}
