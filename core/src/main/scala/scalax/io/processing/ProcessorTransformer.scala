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
          private[this] val wrapped = opened.execute()
          def hasNext = wrapped.hasNext
          def next = wrapped.next
          def doClose = opened.cleanUp
        }
      }
    }
  }/*
  implicit def resourceTransformer[A] = new ProcessorTransformer[A, A, resource.ManagedResource[A]] {
    def transform(from: Processor[A]) = new resource.ManagedResourceOperations[A] {
      def acquireFor[U](f:A => U):Either[List[Throwable],U] = {
        val created = from.init
        var errors:List[Throwable] = Nil
        
        val result = util.control.Exception.allCatch[U].either(f(created.execute()))
          f()
        }
      }
    }
  }*/
}