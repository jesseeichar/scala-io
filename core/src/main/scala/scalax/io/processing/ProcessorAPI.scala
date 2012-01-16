package scalax.io
package processing

class ProcessorAPI[+A](private[this] var iter: CloseableIterator[A]) {
  private[this] var ops = CloseableIteratorOps(iter)
  private[this] def updateIter(f: => CloseableIterator[A]) = {
    iter = f
    ops = CloseableIteratorOps(iter)
  }
  private[processing] def iterator = iter
  private[this] def createSideEffect(f: => Unit) = new Processor[Unit] {
    private[processing] def init = new Opened[Unit] {
      def cleanUp() = ()
      def execute() = Some(f)
    }
  }
  
  private[this] def createSeq[U](f: => Iterator[U]) = Processor({
    val builder = new collection.immutable.VectorBuilder[U]()
    builder ++= f
    Some(builder.result())
  })
  private[this] def doEnd() = iter = iter.take(0)
  
  def takeWhile(f: A => Boolean) = createSeq[A](iter takeWhile f)
  def take(i: Int) = createSeq[A](iter take i)
  def drop(i: Int) = createSideEffect(
      updateIter(ops.drop(i)))
  def end() = createSideEffect(doEnd())
  def endWhen(f: => Boolean) = createSideEffect(if(f) doEnd())
  def next = Processor(if(iter.hasNext) Some(iter.next) else None)
  def nextOption = Processor[Option[A]](Some(if (iter.hasNext) Some(iter.next) else None))
  def repeatUntilEmpty(otherProcessorAPIs: ProcessorAPI[_]*) = RepeatUntilEmpty((this +: otherProcessorAPIs): _*)
  def repeat(times: Int) = Repeat(times)
}
