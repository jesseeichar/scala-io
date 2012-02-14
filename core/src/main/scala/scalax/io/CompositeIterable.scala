package scalax.io
import scala.collection.mutable.Queue

class CompositeIterable[A](builderIterators: Seq[() => Iterator[A]]) {
  def iterator = {
    val iterators = Queue(builderIterators: _*)
    if (iterators.isEmpty) CloseableIterator.empty[A]
    else new CloseableIterator[A] {
      val toClose = Queue.empty[Iterator[A]]
      var now = iterators.dequeue.apply()
      def next = now.next
      def hasNext = {
        if (now.hasNext) true
        else if (iterators.isEmpty) false
        else {
          toClose += now
          now = iterators.dequeue().apply()
          hasNext
        }
      }
      def doClose = {
        import CloseableIterator.safeClose
        safeClose(now) ++ toClose.flatMap(safeClose) ++
            iterators.map(_.apply()).flatMap(safeClose)
      }
    }
  }
}