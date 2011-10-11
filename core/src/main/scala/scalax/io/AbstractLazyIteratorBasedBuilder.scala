package scalax.io
import scala.collection.mutable.Queue
import scala.collection.mutable.Builder

abstract class AbstractLazyIteratorBasedBuilder[A, +Repr] extends Builder[A, Repr]  {
  val builderIterators: Queue[() => Iterator[A]] = Queue.empty
  override def clear() = builderIterators.clear()

  private def addCollector(xs: TraversableOnce[A]) {
    val collector = new Collector
    collector.elements ++= xs
    builderIterators += collector
  }
  override def +=(elem: A): this.type = {
    builderIterators match {
      case bi if bi.nonEmpty && bi.last.isInstanceOf[Collector] =>
        bi.last.asInstanceOf[Collector].elements += elem
      case _ => addCollector(Iterator.single(elem))
    }
    this
  }
  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs match {
      case lt: LongTraversableLike[_, _] => builderIterators += (() => lt.iterator)
      case xs if xs.isTraversableAgain => builderIterators += (() => xs.toIterator)
      case xs if builderIterators.nonEmpty && builderIterators.last.isInstanceOf[Collector] =>
        builderIterators.last.asInstanceOf[Collector].elements ++= xs
      case xs => {
        addCollector(xs)
      }
    }
    this
  }
  class Collector extends Function0[Iterator[A]] {
    val elements = Queue.empty[A]
    def apply() = elements.toIterator
  }
}
