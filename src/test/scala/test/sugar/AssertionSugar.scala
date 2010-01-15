/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar
import scala.reflect.{
  Manifest, ClassManifest
}
import ClassManifest.singleType
import org.junit.Assert.fail

trait AssertionSugar {
  def intercept[E <: Throwable](test : => Unit)(implicit m:Manifest[E]) : Unit = {
    try {
      test
      fail("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e if (m >:> singleType(e)) => ()
      case e => fail("Expected "+m.toString+" but instead got "+e.getClass)
    }
  }
}
