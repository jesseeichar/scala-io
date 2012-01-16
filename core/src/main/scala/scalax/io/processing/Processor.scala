package scalax.io
package processing;

trait Processor[+A] {
  self =>
  
  def traversable[B](implicit transformer: ProcessorTransformer[B, A, LongTraversable[B]]): LongTraversable[B] = transformer.transform(this)

  def acquireAndGet[U](f: A => U): Option[U] = {
    val initialized = init
    try initialized.execute.map(f)
    finally initialized.cleanUp
  }

  private[processing] def init: Opened[A]
  def filter(f:A => Boolean) = new WithFilter(this,f) 
  def withFilter(f:A => Boolean) = new WithFilter(this,f) 
  def foreach[U](f: A => U): Unit = acquireAndGet(f)
  def map[U](f: A => U) = Processor[Opened[A], U](init, in => in.execute.map(f), _.cleanUp)
  def flatMap[U](f: A => Processor[U]) = {
    Processor[(Opened[A], Option[Opened[U]]), U]({
      val currentOpenProcessor = self.init
      (currentOpenProcessor, currentOpenProcessor.execute.map(f(_).init))
    },
      _._2.flatMap(_.execute),
      in => { in._2.map(_.cleanUp); in._1.cleanUp() })
  }
}
private[processing] trait Opened[+A] {
  def execute: Option[A]
  def cleanUp(): Unit
}



object Processor {
  def apply[B, A](before: => B, valueFactory: B => Option[A], after: B => Unit) = new Processor[A] {
    private[processing] def init = {
      val resource = before
      new Opened[A] {
        def execute() = valueFactory(resource)
        def cleanUp() = after(resource)
      }
    }
  }
  def apply[A](valueFactory: => Option[A]) = new Processor[A] {
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
    val processorAPI = new ProcessorAPI(iterInstance)
    new Opened[ProcessorAPI[A]] {
      def execute() = Some(processorAPI)
      def cleanUp() = iterInstance.close()
    }
  }
}

private[processing] class WithFilter[+A](base:Processor[A], filter: A=>Boolean) extends Processor[A] {
    private[processing] def init = base.init

    override def foreach[U](f: A => U): Unit = 
      base.init.execute.filter(filter).foreach(f)
    override def map[U](f: A => U) = 
      Processor(base.init.execute.filter(filter).map(f))
    override def flatMap[U](f: A => Processor[U]) = 
      Processor(base.init.execute.filter(filter).flatMap(f(_).init.execute))
}