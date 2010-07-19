/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.ramfs

import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder

import scalaio.test.FileSystemFixture

trait RamFixture {

  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = {
    fixture = new RamFileSystemFixture()
    assert(fixture != null)
  }

  @After
  def after() : Unit = fixture.after()

}