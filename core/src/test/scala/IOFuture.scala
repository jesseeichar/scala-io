import scala.concurrent._
import util.Duration
import scala.util.{ Try, Success, Failure }

class IOFuture[T](iter: TestIter, seed: T)(process: (Byte, T) => (Boolean, T)) extends Future[T] {
  private[this] val privateValue = new SyncVar[Try[T]]()
  private[this] var state = (true, seed)
  def next(): Unit = {
    try {
      while (iter.hasNext && state._1) {
        state = process(iter.next, state._2)
      }

      if (!state._1 || !iter.available) {
        val value = Success(state._2)
        privateValue put value
        handlers.foreach(_._1.apply(value))
      } else {
        iter.load(_ => next())
      }
    } catch {
      case t: Throwable => privateValue.put(Failure(t))
    }
  }

  next()
  private[this] var handlers = Vector[(Try[T] => Any, ExecutionContext)]()

  def isCompleted = privateValue.isSet
  def onComplete[U](func: (Try[T]) ⇒ U)(implicit executor: ExecutionContext): Unit = {
    handlers = handlers :+ (func -> executor)
  }
  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    val result = privateValue.get(atMost.toMillis)
    if (result.isEmpty) throw new TimeoutException()
    this
  }
  def result(atMost: Duration)(implicit permit: CanAwait): T = {
    privateValue.get(atMost.toMillis).map(_.get) getOrElse (throw new TimeoutException())
  }

  def value: Option[Try[T]] = if (privateValue.isSet) Some(privateValue.take) else None
}

object IOFuture extends App {
  val i = new TestIter("/tmp/dep")
  implicit val c = ExecutionContext.global
  val future = new IOFuture[Long](i, 0)(
    (b, t) => {
      if (t % 1000 == 0) print('.')
      if (t % 1000000 == 0) print("\n"+(t/1000000))
      (true, t + 1)
    })
  future.onComplete(_ => i.close)
  var done = new SyncVar[Boolean]
  future.onComplete {
    case Success(j) =>
      println("\nSuccess: " + j)
      Thread.sleep(500)
      done put true
    case Failure(e) =>
      println("\nFailure")
      e.printStackTrace
      Thread.sleep(500)
      done put true
  }
  done.take()
}