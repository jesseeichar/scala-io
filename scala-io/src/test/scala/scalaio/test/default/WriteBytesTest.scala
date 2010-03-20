/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import Path.AccessModes._
import OpenOption._

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

    final val DEFAULT_DATA = "to be overwritten"

    @Before
    def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())
  
    @After
    def after() : Unit = fixture.after()

    @Test
    def create_file_by_default() : Unit = {
        val (file, ops) = create()
        val bytes = DEFAULT_DATA.getBytes
      
        assertTrue(file.exists)
        assertEquals(bytes.size, file.size)
      
        assertArrayEquals(bytes, ops.slurpBytes)
    }

    @Test
    def overwrite_existing_file_by_default() : Unit = {
        val (file,ops) = create()
        val bytes = "hello".getBytes
        ops writeBytes bytes
      
        assertEquals(Some(bytes.size), ops.size)
        assertArrayEquals(bytes, ops.slurpBytes)
    }
  
    @Test
    def append_data_to_existing_file() : Unit = {
        val (file,ops) = create()
        val startBytes = DEFAULT_DATA.getBytes
        val bytes = "hello".getBytes

        ops writeBytes (bytes, Seq(APPEND))
        
        assertEquals(Some(startBytes.size + bytes.size), ops.size)
        assertArrayEquals(startBytes ++ bytes, ops.slurpBytes)
  }
  
  
  private def create(data : String = DEFAULT_DATA) = {
      val file = fixture.path
      val ops = file.fileOps
      
      ops writeBytes data.getBytes
      assertTrue(file.exists)
      
      (file,ops)
  }
  
  
}