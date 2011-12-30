package scalax.io

class OpenedLongTraversable[+A](traversable:LongTraversable[A]) {
	def flatMap[U](f: LongIterator[A] => OpenedLongTraversable[U]):OpenedLongTraversable[U] = null
    def map[U](f: LongIterator[A] => U):OpenedLongTraversable[U] = null
	def withFilter(f: LongIterator[A] => Boolean):OpenedLongTraversable[A] = null
}