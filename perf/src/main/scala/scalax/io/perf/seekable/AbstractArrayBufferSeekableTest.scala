package scalax.io.perf
package seekable

import scala.collection.mutable.ArrayBuffer
import scalax.io._
import Resource._
import StandardOpenOption._
import Line.Terminators._
import scalax.io.ArrayBufferSeekableChannel
import scalax.io.OpenOption
import scalax.io.SeekableByteChannel
import Utils._
abstract class AbstractArrayBufferSeekableTest extends AbstractSeekableTest {
	type Source = ArrayBuffer[Byte]
	  def setup(size:Int, 
      lines: Int = 2, 
      term: String = NewLine.sep):Source = {
      ArrayBuffer(generateTestData(size, lines, term).getBytes(Codec.UTF8.name):_*)
  }
  
  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(source:Source, 
      openOptions: Seq[OpenOption] = ReadWrite):() => SeekableByteChannel = () => {
    new ArrayBufferSeekableChannel(source,openOptions:_*)(_=>(),_=>()):SeekableByteChannel
  }
}