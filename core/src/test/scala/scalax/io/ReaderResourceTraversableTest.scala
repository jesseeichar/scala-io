/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.Reader
import scalax.io.managed.ReaderResource


class ReaderResourceTraversableTest extends ResourceTraversableTest {

  override def expectedData(tsize: Int,
                            dataFunc: (Int) => Seq[Int]) =
    dataFunc(tsize) mkString "" map {
      _.toInt
    }

  override def traversable[U, A](tsize: Int,
                                 callback: (Int) => U,
                                 dataFunc: (Int) => Traversable[Int],
                                 conv: (Int) => A,
                                 closeFunction: () => Unit = () => (),
                                 resourceContext: ResourceContext) = {
    def callBackAndConv = (c: Char) => {
      val i = c.toInt
      callback(i)
      conv(i)
    }
    def reader = new java.io.StringReader(dataFunc(tsize) mkString "") {
      override def close() = closeFunction()
    }
    def resource = Resource.fromReader(reader).updateContext(resourceContext)
    ResourceTraversable.readerBased(resource.open, resource.context, initialConv = callBackAndConv)
  }

}
