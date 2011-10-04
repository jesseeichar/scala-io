/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import org.junit.Assert._
import org.junit.{
  Test,
  Ignore
}
import java.io.ByteArrayInputStream
import java.io.InputStream
class ResourceTraversableViewTest extends ResourceTraversableTest {

  override def traversable[U, A](tsize: Int,
    callback: (Int) => U,
    dataFunc: (Int) => Traversable[Int],
    conv: (Int) => A): LongTraversable[A] =
    super.traversable(tsize, callback, dataFunc, conv)

  @Test
  def bytes_of_a_Resource_must_be_a_ResourceTraversableView {
    var count = 0
    val tmpIn = new InputStream(){
      var p = 10
      def read = {
        count += 1
        p -= 1
        p
      }
    }
    val resource = Resource.fromInputStream(tmpIn)
    val bytes = resource.bytes
    
    assertEquals(9,bytes.head)
    assertEquals(1,count)
  }

}
