package scalax.io
import scala.annotation.tailrec

/**
 * Used to transform LongTraversables in a declarative fashion.  
 * 
 * The desire is to declaratively define how to read the LongTraversable and the pattern will be applied repeatedly to the data contained in 
 * the LongTraversable until all data has been consumed by the pattern.
 * 
 * Calling foreach, map or flatMap will provide access to the iterator.  Unlike the typical foreach/map/flatmap, the block is called repeated with the same
 * iterator object until all elements have been accessed by the code inside the block.  Suppose if the block had iter.next.  Then the block would be called once 
 * for each element in the iterator.  Because if this it is possible to create an infinate loop if the block does not read any elements from the iterator.
 * 
 * In the case of map and flatMap the data will be lazily processed. For example.  if you perform a map the mapping will not be performed until the traversable is
 * obtained and a method triggering the traversal of all the elements is performed
 * 
 * Consider a file that contains data with the following organization (the | are for visual representation only):
 * {{{
 *   100 byte header|1byte row header|row data
 * }}}
 * 
 * the 100 byte header might have information like number of rows in file and file author
 * each row would start with a single byte that declares how big the row is.
 * the row data is a UTF-8 encoded string
 * 
 * If one would like to iterate through each record (row), the implementation could be as follows (using the file API):
 * 
 * {{{
 *   import scalax.io.{Resource,Codec,LongTraversable}
 *
 *   // drop the header because we are not interested in it now
 *   val bytes = Resource.fromFile("somefile").bytes.drop(100)
 *   val rowTransformer = for (iter <- bytes.tranformer) yield {
 *     // this block is called as long as the data remains
 *     // get one byte.  This is the row header (and indicates the amount of row data)
 *     val rowLength = iter.next.toInt
 *     // read the row data
 *     val rowData = iter.take(rowLength)
 *     // Convert the rows data to a string
 *     new String(rowData.toArray, Codec.UTF8.charSet)
 *   }
 *   
 *   // rowTranformer is designed to be used to define the structure of a file or other data
 *   // it does not actually process the file.
 *   // At this point the file has not been opened yet
 *   val rowTraversable:LongTraversable[String] = rowTransformer.traversable
 *   
 *   // Since LongTraversable's are lazy, the file still has not been opened
 *   // only the actual calling of foreach (next line) will trigger the file read.
 *   rowTraversable.foreach(println)
 * } 
 * }}}
 * 
 * Look at the long-traversable-transformations examples for more examples
 */
class LongTraversableTransformer[+A](val traversable: LongTraversable[A]) {
  def foreach[U](f: LongIterator[A] => U) = traversable.withIterator { iter =>
    while (iter.hasNext) f(iter)
  }
  
  def flatMap[U](f: LongIterator[A] => LongTraversableTransformer[U]): LongTraversableTransformer[U] = new LongTraversableTransformer(new LongTraversable[U]{
    def iterator = new CloseableIterator[U] {
      val iterUnderProcess = traversable.iterator
      var nextElem:CloseableIterator[U] = CloseableIterator.empty

      @tailrec
      final def hasNext = {
        if(nextElem.hasNext) true
        else if(iterUnderProcess.hasNext){
          nextElem = f(iterUnderProcess).traversable.iterator
          hasNext
        } else {
          false
        }
      }

      final def next = {
        if(!hasNext) throw new NoSuchElementException("No more elements available")
        nextElem.next()
      }

      final def doClose = {
        nextElem.close()
       iterUnderProcess.close() 
      }
    }
  })

  def map[U](f: LongIterator[A] => U): LongTraversableTransformer[U] = new LongTraversableTransformer(new LongTraversable[U]{
    def iterator = new CloseableIterator[U] {
      val iterUnderProcess = traversable.iterator
      var nextElem = null.asInstanceOf[U]
      final def hasNext = {
        if(nextElem != null) true
        else if(iterUnderProcess.hasNext){
          nextElem = f(iterUnderProcess)
          nextElem != null
        } else {
          false
        }
      }
      final def next = {
        if(!hasNext) throw new NoSuchElementException("No more elements available")
        val n = nextElem
        nextElem = null.asInstanceOf[U]
        nextElem
      }
              
      final def doClose = iterUnderProcess.close();
    }
  })
}