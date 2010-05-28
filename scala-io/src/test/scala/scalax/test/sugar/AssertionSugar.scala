/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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
    val error = try {
      test
      Some("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e:AssertionError if m.erasure != classOf[AssertionError] => throw e
      case e if (m >:> singleType(e)) => None
      case e => 
        e.printStackTrace
        Some("Expected "+m.toString+" but instead got "+e.getClass)
    }
    
    error match {
      case Some(msg) => fail(msg)
      case None => ()
    }
  }
  
  def repeat[U] (f : => U)(implicit times : Int = 50) = 1 to times foreach {_ => f}
}
