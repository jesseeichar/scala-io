/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder


trait Fixture {

  def createFixture() : FileSystemFixture

  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = {
    fixture = createFixture()
    assert(fixture != null)
  }

  @After
  def after() : Unit = fixture.after()

}