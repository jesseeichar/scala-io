/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.file.Path.AccessModes._
import org.junit.{
  Before, After
}
import scalax.file.FileSystem

trait Fixture {

  def createFixture() : FileSystemFixture

  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = {
    fixture = createFixture()
    assert(fixture != null)
  }

  @After
  def after() : Unit = try {fixture.after()} catch {case  e => println("error in after:"+e)}

  def isWindows = (System.getProperty("os.name").toLowerCase startsWith "windows") && fixture.fs == FileSystem.default
  def permissions(modes:AccessMode*)=
    if(isWindows) Set(modes:_*) ++ Set(Read, Execute)
    else Set(modes:_*)

}
