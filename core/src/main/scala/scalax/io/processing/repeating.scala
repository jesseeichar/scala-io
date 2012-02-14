package scalax.io
package processing

/**
 * Processor that repeats until empty or maxRepetitions
 */
private[processing] class RepeatUntilEmpty(private[this] val maxRepetitions:Long, processorFactory: ProcessorFactory, private[this] val ProcessorAPIs: ProcessorAPI[_]*) {
  private[this] def iter = new CloseableIteratorOps(new CloseableIterator[Long] {
    private[this] var index = 0L
    def hasNext = index < maxRepetitions && ProcessorAPIs.exists{api =>
      api.iterator.hasNext
    }
    def next = {
      index += 1
      index - 1
    }
    def doClose = ()
  })

  def foreach[U](f: Long => U) = iter.iter foreach f
  def flatMap[U](f: Long => Processor[U]) = processorFactory[Iterator[U]](Some(iter.flatMap(i => f(i).init.execute)))
  def map[U](f: Long => U) = processorFactory[Iterator[U]](Some(iter map f))
}
