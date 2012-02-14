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
    def transform(from: Processor[Iterable[A]]) = iteratorToLongTraverableTransformer[A].transform(from.map(_.iterator))
  }
  implicit def iteratorToLongTraverableTransformer[A] = new ProcessorTransformer[A, Iterator[A], LongTraversable[A]] {
    def transform(from: Processor[Iterator[A]]) = new LongTraversable[A] {
      def context = from.context
      def iterator = {
        val opened = from.init
        new CloseableIterator[A] {
          private[this] val wrapped = opened.execute.orNull
          /* A currently stupid implementation of WithFilter returns null if the element is filtered
             out so nextElem finds the next non-null element or is null */
          private[this] var nextElem:A = null.asInstanceOf[A]
          def hasNext = {
            if(wrapped == null) false
            else if(nextElem != null) true
            else if(!wrapped.hasNext) false
            else {
              nextElem = wrapped.next
              hasNext
            }
          }
          def next = {
            val n = nextElem
            nextElem = null.asInstanceOf[A]
            n
          }
          def doClose = opened.cleanUp
        }
      }
    }
  }
}