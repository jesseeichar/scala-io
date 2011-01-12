/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import org.junit.Assert._
import org.junit.{
  Test, Ignore
}


class ReaderResourceTraversableViewTest extends ResourceTraversableViewTest {

  protected override def expectedData : Traversable[Int] = super.expectedData mkString "" map {_.toInt}

    override def newResource[A](conv:Int=>A) = {
      val data = super.expectedData mkString ""
      def resource = Resource.fromReader(new java.io.StringReader(data))
      ResourceTraversable.readerBased(resource, _conv = (c:Char) => conv(c.toInt)).view
    }

}
