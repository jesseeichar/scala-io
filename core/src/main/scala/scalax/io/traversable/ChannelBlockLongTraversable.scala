package scalax.io
package traversable
import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import annotation.switch

private abstract class State { val id: Int }
private object ReadyState extends State { final val id = 0 }
private object ContinueState extends State { final val id = 1 }
private object EmptyState extends State { final val id = 2 }

class ChannelBlockLongTraversable(blockSize: Option[Int], val context: ResourceContext, sizeFunc:()  => Option[Long], opener: => OpenedResource[ReadableByteChannel]) extends LongTraversable[ByteBlock] {
  self =>

  protected[io] def iterator: CloseableIterator[ByteBlock] = new CloseableIterator[ByteBlock] {
    private[this] val opened = opener
    private[this] val channel = opened.get
    private[this] val context = opened.context
    private[this] var buffer = blockSize match {
      case Some(size) => context.createNioBuffer(size, Some(channel), true)
      case None => context.createNioBuffer(sizeFunc(), Some(channel), true)
    }
    // bytes read the last read.  -1 
    private[this] var state: State = ContinueState
    private[this] var block = new ByteBufferWrapperByteBlock(buffer)

    def next(): ByteBlock = {
      state = ContinueState
      block.set()
      block
    }
    def hasNext: Boolean = {
      (state.id: @switch) match {
        case ReadyState.id => true
        case EmptyState.id => false
        case ContinueState.id =>
          buffer.clear()
          state = if (channel.read(buffer) < 0) EmptyState else ReadyState
          buffer.flip()
          state == ReadyState
      }
    }
    protected def doClose() = opened.close()
  }

  override def force = new LongTraversable[ByteBlock] {
    def context = self.context
    private[this] val data = self.withIterator {
       _.foldLeft(Vector[ByteBlock]()) { (acc, next) =>
          acc :+ next.force
        }
    }
    
    def iterator = CloseableIterator(data.iterator)
  }
}

private class ByteBufferWrapperByteBlock(private[this] val buffer: ByteBuffer) extends ByteBlock {

  private[this] var _size = 0
  @inline
  def size = _size
  @inline
  final def apply(i: Int) = buffer.get(i)
  def set() = _size = buffer.limit
}