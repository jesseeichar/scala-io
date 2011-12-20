package scalax.io
package traversable
import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import annotation.switch
import java.io.InputStream

class InputStreamBlockLongTraversable(blockSize: Option[Int], opener: => OpenedResource[InputStream]) extends LongTraversable[ByteBlock] {

  protected[io] def iterator: CloseableIterator[ByteBlock] = new CloseableIterator[ByteBlock] {
    val opened = opener
    val inputStream = opened.get
    var buffer = Buffers.arrayBuffer(blockSize.map { _.toLong })
    // bytes read the last read.  -1 
    var state: State = ContinueState
    var block = new ArrayWrapperByteBlock(buffer)

    def next(): ByteBlock = {
      state = ContinueState
      block
    }
    def hasNext: Boolean = {
      (state.id: @switch) match {
        case ReadyState.id => true
        case EmptyState.id => false
        case ContinueState.id =>
          block.set(inputStream.read(buffer))
          state = if (block.size < 0) EmptyState else ReadyState
          state == ReadyState
      }
    }
    protected def doClose(): Unit = opened.close()
  }
}

private class ArrayWrapperByteBlock(buffer:Array[Byte]) extends ByteBlock{
  private[this] var _size = 0
  @inline
  def size = _size
  @inline
  final def apply(i: Int) = buffer(i)
  def set(size:Int) = _size = size 
}