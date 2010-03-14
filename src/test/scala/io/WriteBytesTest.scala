/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.IOException

class WriteBytesTest extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8
  
  var fixture : FileSystemFixture = _

  @Before
  def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())
  
  @After
  def after() : Unit = fixture.after()

  @Test
  def write_bytes_should_overwrite_and_append_binary_data() : Unit = {
      val file = fixture.file
      val bytes = "hello".getBytes
      file.fileOps writeBytes "hello".getBytes
      assertEquals(bytes.size, file.fileOps.size)
  }
  
  
}