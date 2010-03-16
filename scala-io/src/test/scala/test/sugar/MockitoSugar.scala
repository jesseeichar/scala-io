/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar

trait MockitoSugar {
  def mock[T](implicit manifest:scala.reflect.Manifest[T]):T = org.mockito.Mockito.mock(manifest.erasure).asInstanceOf[T]
}

object MockitoSugarSupport extends MockitoSugar