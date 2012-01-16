package scalax.io
package processing

trait ProcessorTransformer[+A, -From, +To] {
  def transform(from: Processor[From]): To
}
object ProcessorTransformer {
  implicit def longTraversableTransformer[A] = new ProcessorTransformer[A, Iterator[A], LongTraversable[A]] {
    def transform(from: Processor[Iterator[A]]) = new LongTraversable[A] {
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