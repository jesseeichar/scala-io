package scalax.io

class OpenedLongTraversable[+A](traversable: LongTraversable[A]) {
  def foreach[U](f: LongIterator[A] => U) = traversable.withIterator { iter =>
    while (iter.hasNext) {
      f(iter)
    }
  }
  
  def flatMap[U](f: LongIterator[A] => OpenedLongTraversable[U]): OpenedLongTraversable[U] = null
  def map[U](f: LongIterator[A] => U): OpenedLongTraversable[U] = null
  def withFilter(f: LongIterator[A] => Boolean): OpenedLongTraversable[A] = null
}