package scalax.io

trait LongIterator[@specialized(Byte,Char) +A] extends Iterator[A] {
  def ltake(i: Long):LongIterator[A]
  def ldrop(i: Long):LongIterator[A]
  def lslice(from: Long, until: Long):LongIterator[A]
  def close():Unit
}