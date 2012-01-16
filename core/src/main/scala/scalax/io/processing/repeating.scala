package scalax.io
package processing


private[processing] case class RepeatUntilEmpty(ProcessorAPIs: ProcessorAPI[_]*) {
  private[this] def iter = new CloseableIteratorOps(new CloseableIterator[Int] {
    private[this] var index = 0
    def hasNext = ProcessorAPIs.exists{api =>
      val iter = api.iterator
      iter.hasNext
      }
    def next = {
      index += 1
      index - 1
    }
    def doClose = ()
  })

  def foreach[U](f: Int => U) = iter.iter foreach f
  def flatMap[U](f: Int => Processor[U]) = Processor[CloseableIterator[U]](Some(iter.flatMap(i => f(i).init.execute)))
  def map[U](f: Int => U) = Processor[CloseableIterator[U]](Some(iter map f))
}

private[processing] case class Repeat(times: Int) {
  def foreach[U](f: Int => U) = 1 to times foreach f
  def flatMap[U](f: Int => Processor[U]) = Processor[Iterator[U]](Some((1 to times).toIterator.flatMap{i =>
    val nextProcessor = f(i)
    val nextInit = nextProcessor.init
    nextInit.execute
  }))
  def map[U](f: Int => U) = Processor[Iterator[U]](Some((1 to times).toIterator map f))
}
