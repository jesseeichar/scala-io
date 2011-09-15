package scalax.io
package traversable
import java.io.InputStream

class InputStreamResourceTraversable(
  resourceOpener: => OpenedResource[InputStream],
  sizeFunc: () => Option[Long],
  val start: Long,
  val end: Long)
  extends LongTraversable[Byte]
  with LongTraversableLike[Byte, LongTraversable[Byte]] {

  protected[io] def iterator: CloseableIterator[Byte] = new CloseableIterator[Byte] {
    val buffer = Buffers.arrayBuffer(sizeFunc())
    val openResource = resourceOpener
    val inConcrete = openResource.get
    inConcrete.skip(start)
    var read = inConcrete.read(buffer)
    var i = 0
    def hasNext = {
      if (i < read) true
      else {
        i = 0
        read = inConcrete.read(buffer)
        i < read
      }
    }
    @specialized(Byte)
    def next = {
      i += 1
      buffer(i - 1)
    }
    def doClose() = openResource.close()
  }

  override lazy val hasDefiniteSize= sizeFunc().nonEmpty
  override def lsize = sizeFunc() match {
    case Some(size) => size
    case None => super.size
  }
  override def size = lsize.toInt

}