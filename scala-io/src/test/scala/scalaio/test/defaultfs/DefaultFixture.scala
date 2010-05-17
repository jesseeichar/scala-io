/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.defaultfs

import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder

import scalaio.test.FileSystemFixture

trait DefaultFixture {

  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = {
    fixture = new DefaultFileSystemFixture(new TemporaryFolder())
    assert(fixture != null)
  }

  @After
  def after() : Unit = fixture.after()

}