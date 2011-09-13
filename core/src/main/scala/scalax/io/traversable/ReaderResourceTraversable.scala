package scalax.io
package traversable
import java.io.Reader

import scalax.io.CloseableIterator
import scalax.io.LongTraversable
import scalax.io.LongTraversableLike
import scalax.io.OpenedResource

class ReaderResourceTraversable (
  resourceOpener: => OpenedResource[Reader],
  val start:Long,
  val end:Long)
  extends LongTraversable[Char]
  with LongTraversableLike[Char, LongTraversable[Char]] {

  protected[io] def iterator: CloseableIterator[Char] = new CloseableIterator[Char] {
    val buffer = Buffers.readerBuffer
    val openResource = resourceOpener
    val inConcrete = openResource.get
    inConcrete.skip(start)
    var read = inConcrete.read(buffer)
    var i = 0
    @inline
    def hasNext = {
      if(i < read) true
      else {
        i=0
        read = inConcrete.read(buffer)
        i < read
      }
    }
    @inline @specialized(Char)
    def next = {
      i += 1
      buffer(i-1)
    }
    def doClose() = openResource.close()
  }

}