/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
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
import scalax.test.sugar.AssertionSugar

trait Fixture {

  def createFixture() : FileSystemFixture

  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = {
    fixture = createFixture()
    assert(fixture != null)
    def testFile(javaRoot:java.io.File) = {
      javaRoot.getCanonicalPath == fixture.root.toRealPath().path
    }
    assert(fixture.root.fileSystem != FileSystem.default || !(java.io.File.listRoots().exists(testFile)), "Root cannot be the true file system root because some tests delete root which could be a major issues as it could delete entire filesystem")
  }

  @After
  def after() : Unit = try {fixture.after()} catch {case  e => println("error in after:"+e)}

  def isWindows = AssertionSugar.isWindows && fixture.fs == FileSystem.default
  def permissions(modes:AccessMode*)=
    if(isWindows) Set(modes:_*) ++ Set(Read, Execute)
    else Set(modes:_*)

}
