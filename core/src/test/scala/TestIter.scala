import java.nio.channels._
import java.nio._
import java.nio.file._
import scala.concurrent._
import scala.util.{Try, Success, Failure}
import util.Duration

class TestIter(file:String) {
  val buffer = ByteBuffer.allocateDirect(8 * 1024)
  val channel = AsynchronousFileChannel.open(Paths.get(file))
  def available = channel.isOpen()
  def load [U](listener: Try[Integer] => U) = synchronized {
    if(loadListener != null) throw new ReadPendingException()
    loadListener = listener
    buffer.clear
    channel.read(buffer, 0, null, handler)
  }
  def hasNext = buffer.remaining > 0
  def next = buffer.get()
  
  private[this] var loadListener:Try[Integer] => Any = _
  private[this] val handler = new CompletionHandler[Integer, Null] {
    def completed(bytesRead: Integer, p2: Null): Unit = {
      val listener = synchronized {
    	  val l = loadListener
	      loadListener = null
	      l
      }
      listener.apply(Success(bytesRead))
    }
     def failed(exc: Throwable, attachment: Null): Unit = synchronized {
       val listener = synchronized {
    	  val l = loadListener
	      loadListener = null
	      l
      }
      listener.apply(Failure(exc))
     }
  }
}

class LoadFuture extends Future[Integer] with CompletionHandler[Integer, Null] {
  private[this] var privateValue:Option[Try[Integer]] = None
  private[this] var handlers = Vector[(Try[Integer] => Any, ExecutionContext)]()
  def completed(bytesRead: Integer, p2: Null): Unit = synchronized {
    val value = Success(bytesRead)
    privateValue = Some(value)
    handlers.foreach{f =>
      try {
        f._1(value)
      } catch {
        case t: Throwable => f._2.reportFailure(t)
      }
    }
  }
  def failed(exc: Throwable, attachment: Null): Unit = synchronized {
    val value = Failure(exc)
    privateValue = Some(value)
    handlers.foreach{f =>
      try {
        f._1(value)
        f._2.reportFailure(exc)
      } catch {
        case t: Throwable => f._2.reportFailure(t)
      }
    }
  }
  def isCompleted = synchronized {privateValue.isDefined}
  def onComplete[U](func: (Try[Integer]) ⇒ U)(implicit executor: ExecutionContext): Unit = {
    handlers = handlers :+ (func -> executor)
  }
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    // TODO 
    this
  }
  
  def result(atMost: Duration)(implicit permit: CanAwait): Integer = synchronized {
    ready(atMost)
    privateValue.get.get
  }
  
  def value: Option[Try[Integer]] = synchronized {privateValue}
}
