package scalax.io
package traversable
import java.io.Reader

import scalax.io.CloseableIterator
import scalax.io.LongTraversable
import scalax.io.LongTraversableLike
import scalax.io.OpenedResource

private[io] class ReaderResourceTraversable (
  resourceOpener: => OpenedResource[Reader],
  resourceContext: ResourceContext,
  val start:Long,
  val end:Long)
  extends LongTraversable[Char]
  with LongTraversableLike[Char, LongTraversable[Char]] {

  def context = resourceContext
  protected[io] def iterator: CloseableIterator[Char] = new CloseableIterator[Char] {
    private[this] val openResource = resourceOpener
    private[this] val buffer = new Array[Char](openResource.context.charBufferSize(None, true))
    private[this] val inConcrete = openResource.get
    inConcrete.skip(start)
    private[this] var read = inConcrete.read(buffer)
    private[this] var i = 0

    def hasNext = {
      if(i < read) true
      else {
        i=0
        read = inConcrete.read(buffer)
        i < read
      }
    }
    def next = {
      i += 1
      buffer(i-1)
    }
    def doClose() = openResource.close()
  }

}
