package scalaio.test

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import org.junit.Assert._
import org.junit.Test
import scalax.file.PathMatcherFactory._
import scalax.file.{PathMatcher, Path, PathMatcherFactory}
import scalax.file.PathMatcher.NameIs
import scalax.test.sugar.FSAssertionSugar

class PathMatcherFactoryTest extends FSAssertionSugar {

  @Test
  def `function path matcher converts a function to a PathMatcher` : Unit = {
    val fs = zipfs
    val namePath = fs("world","name")
    val notNamePath = fs("world","not")
    val nameis = FunctionToMatcher{_.name == "name"}
    val notnameis = - FunctionToMatcher{_.name == "name"}
    assertTrue(nameis(namePath))
    assertFalse(nameis(notNamePath))
    assertTrue(notnameis(notNamePath))
  }

  @Test
  def `function path matcher converts a PathMatcher to a PathMatcher` : Unit = {
    val fs = zipfs
    val namePath = fs("world","name")
    val notNamePath = fs("world","not")
    val nameis = FunctionToMatcher(FunctionToMatcher{_.name == "name"})
    val notnameis = FunctionToMatcher(- FunctionToMatcher{_.name == "name"})
    assertTrue(nameis(namePath))
    assertFalse(nameis(notNamePath))
    assertTrue(notnameis(notNamePath))
  }

  def performAssertion[F](f:F = PathMatcher.All, path:Path, expectation:Boolean)(implicit fac:PathMatcherFactory[F]) = {
    assert(expectation == fac(f)(path))
  }


    @Test
    def `method with implicit factory and default` : Unit = {
      val fs = zipfs
      val namePath = fs("world","name")
      val notNamePath = fs("world","not")
      val nameis = NameIs("name")
      val notnameis = - NameIs("name")
      performAssertion(nameis,namePath,true)
      performAssertion(nameis,notNamePath,false)
      performAssertion(notnameis,notNamePath,true)
      performAssertion(notnameis,namePath,false)
    }


}
