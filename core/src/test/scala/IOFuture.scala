import scala.concurrent._
import util.Duration
import scala.util.{Try, Success, Failure}

class IOFuture[T](iter: TestIter) extends Future[T]{
  private[this] val privateValue = new SyncVar[Try[T]]()
  
  private[this] var handlers = Vector[(Try[T] => Any, ExecutionContext)]()
  
  def isCompleted = privateValue.isSet
  def onComplete[U](func: (Try[T]) ⇒ U)(implicit executor: ExecutionContext): Unit = {
    handlers = handlers :+ (func -> executor)
  }
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    // TODO 
    this
  }
  
  def result(atMost: Duration)(implicit permit: CanAwait): T = synchronized {
    // TODO
  }
  
  def value: Option[Try[T]] = if(privateValue.isSet) Some(privateValue.take) else None
}