package scalax.io
package processing

class ProcessorAPI[+A](val iter: CloseableIterator[A]) {
  private[this] def createSideEffect(f: => Unit) = new ProcessorAPI[A](iter) with Processor[Unit] {
    private[processing] def init = new Opened[Unit] {
      def cleanUp() = ()
      def execute() = f
    }
  }
  
  private[this] def createSeq[U](f: => Iterator[U]) = Processor({
    val builder = new collection.immutable.VectorBuilder[U]()
    builder ++= f
    builder.result()
  })
  
  def takeWhile(f: A => Boolean) = createSeq[A](iter takeWhile f)
  def take(i: Int) = createSeq[A](iter take i)
  def drop(i: Int) = createSideEffect(iter.drop(i))
  def next = if (iter.hasNext) Processor(iter.next) else Processor.empty[A]
  def nextOption = Processor[Option[A]](if (iter.hasNext) Some(iter.next) else None)
  def repeatUntilEmpty(otherProcessorAPIs: ProcessorAPI[_]*) = RepeatUntilEmpty((this +: otherProcessorAPIs): _*)
  def repeat(times: Int) = Repeat(times)
}
