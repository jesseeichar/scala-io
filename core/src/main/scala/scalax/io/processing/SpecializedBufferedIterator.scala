package scalax.io
package processing
import scala.collection.immutable.VectorBuilder

private[processing] class SpecializedBufferedIterator[@specialized(Byte,Int,Char) +A](private[this] val sourceIter: CloseableIterator[A]) {
  private[this] var pushedBack:List[A] = Nil
  private[this] var ended: Boolean = false
  
  final def end() = ended = true
  
  /**
   * Will return an Array containing the i elements if i elements remain in the input source. Otherwise an empty
   * array will be returned
   */
  final def takeIfPossible(i:Int) = {
    val result = new VectorBuilder[A]()
    result.sizeHint(i)
    var count = 0
    while(count < i && hasNext) {
      result += next
      count += 1
    }
    if(count < i) {
      result.result() foreach {a => pushedBack = a :: pushedBack}
      Vector.empty
    } else {
      result.result()
    }
  }
  final def takeWhile(f:A => Boolean):Seq[A] = {
    var result = new VectorBuilder[A]()
    var continue = true
    while(continue && hasNext) {
      val nextE = next
      continue = f(nextE)
      if(continue) {
        result += nextE
        nextE
      } else {
        pushedBack = nextE :: pushedBack
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

  final def hasNext =
    !ended && (pushedBack.nonEmpty || sourceIter.hasNext)

  final def next() =
    if (pushedBack.nonEmpty) {
      var next = pushedBack.head
      pushedBack = pushedBack.tail
      next
    } else sourceIter.next()
}