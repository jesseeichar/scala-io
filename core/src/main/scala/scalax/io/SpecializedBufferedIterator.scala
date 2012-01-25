package scalax.io
import scala.collection.immutable.VectorBuilder

private[io] class SpecializedBufferedIterator[@specialized(Byte,Int,Char) +A](private[this] val sourceIter: CloseableIterator[A]) {
  private[this] var hd: A = _
  private[this] var hdDefined: Boolean = false
  private[this] var ended: Boolean = false
  
  final def end() = ended = true
  
  @inline
  final def takeWhile(f:A => Boolean):Seq[A] = {
    var result = new VectorBuilder[A]()
    var continue = true
    while(continue && hasNext) {
      continue = f(head)
      if(continue) {
        result += hd
        next
      }
    }
    result.result()
  }
  final def take(i: Int) = {
    var result = new VectorBuilder[A]()
    var count = 0
    while (count < i && hasNext) {
      result += next
      count += 1
    }
    result.result()
  }
  final def drop(i: Int) = {
    var count = 0
    while (count < i && hasNext) {
      next
      count += 1
    }
  }
  final def head: A = {
    if (!hdDefined) {
      hd = next()
      hdDefined = true
    }
    hd
  }

  final def hasNext =
    !ended && (hdDefined || sourceIter.hasNext)

  final def next() =
    if (hdDefined) {
      hdDefined = false
      hd
    } else sourceIter.next()
}