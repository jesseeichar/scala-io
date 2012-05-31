package scalax.io
package processing

/**
 * Case class to ensure control conversion of a Processor to other objects happens in a type safe way so
 * that resources do not escape and create a resource leak
 */
private[processing] sealed trait ProcessorTransformer[+A, -From, +To] {
  def transform(from: Processor[From]): To
}

private[this] object ProcessorTransformer {
  implicit def iterableToLongTraversableTransformer[A] = new ProcessorTransformer[A, Iterable[A], LongTraversable[A]] {
    def transform(from: Processor[Iterable[A]]) = iteratorToLongTraverableTransformer[A].transform(from.map(t => LongTraversable(from.context, t.iterator)))
  }

  implicit def iteratorToLongTraverableTransformer[A]: ProcessorTransformer[A, LongTraversable[A], LongTraversable[A]] =
    new ProcessorTransformer[A, LongTraversable[A], LongTraversable[A]] {
      def transform(from: Processor[LongTraversable[A]]) = new LongTraversable[A] {
        def context = from.context

        def iterator = {
          val opened = from.init
          new CloseableIterator[A] {
            private[this] val wrapped = opened.execute.map(_.iterator)getOrElse(CloseableIterator.empty)
            def hasNext = wrapped.hasNext
            def next = wrapped.next
            def doClose = opened.cleanUp
          }
        }
      }
    }
}
