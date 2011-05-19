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
  Test, Ignore
}

import java.io._
import scalaio.test.LongTraversableTest
import scalax.io.CloseAction.Noop

class ResourceTraversableTest extends LongTraversableTest {
  override def traversable[U, A](tsize: Int,
                                 callback: (Int) => U,
                                 dataFunc: (Int) => Traversable[Int],
                                 conv: (Int) => A):LongTraversable[A] = {
    def stream = new ByteArrayInputStream(dataFunc(tsize) map {_.toByte} toArray)
    def resource = new CloseableOpenedResource(stream,Noop)
    val callBackAndConv = (i:Byte) => {
      callback(i.toInt)
      conv(i.toInt)
    }
    ResourceTraversable.streamBased(resource, initialConv = callBackAndConv)
  }
}
