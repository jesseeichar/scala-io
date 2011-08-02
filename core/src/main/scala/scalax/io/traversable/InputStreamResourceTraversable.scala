package scalax.io
package traversable
import java.io.InputStream

class InputStreamResourceTraversable(
  resourceOpener: => OpenedResource[InputStream],
  bufferFactory: => Array[Byte] = new Array[Byte](Constants.BufferSize),
  val start:Long,
  val end:Long)
  extends LongTraversable[Byte]
  with LongTraversableLike[Byte, LongTraversable[Byte]] {

  protected[io] def iterator: CloseableIterator[Byte] = new CloseableIterator[Byte] {
    val buffer = bufferFactory
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
    @inline @specialized(Byte)
    def next = {
      i += 1
      buffer(i-1)
    }
    def doClose() = openResource.close()
  }

}