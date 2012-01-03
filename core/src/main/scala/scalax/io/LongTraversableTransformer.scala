package scalax.io

/**
 * Used to transform LongTraversables in a declarative fashion.  The desire is to declaratively define
 * how to read the LongTraversable and the pattern will be applied repeatedly to the data contained in 
 * the LongTraversable until all data has been consumed by the pattern.
 * 
 * Consider a file that contains data with the following organization (the | are for visual representation only):
 * {{{
 *   100 byte header|1byte row header|row data
 * }}}
 * 
 * the 100 byte header might have information like number of rows in file
 */
class LongTraversableTransformer[+A](val traversable: LongTraversable[A]) {
  def foreach[U](f: LongIterator[A] => U) = traversable.withIterator { iter =>
    while (iter.hasNext) f(iter)
  }
  
  def flatMap[U](f: LongIterator[A] => LongTraversableTransformer[U]): LongTraversableTransformer[U] = null
  def map[U](f: LongIterator[A] => U): LongTraversableTransformer[U] = null
}