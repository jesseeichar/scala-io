/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.io.OpenOption._

import org.junit.Assert._
import org.junit.{
  Test, Ignore
}

import java.io.IOException

trait AbstractFileOpsTests extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8

  val demoData = "hello this is demo data"

  /**
   * create a file ops object on a path that is an existing file.  File must be at minimum 5 characters long
   */
  def ops(implicit data : Array[Byte] = demoData.getBytes(codec.name)) : FileOps

  // TODO defaults output
  // TODO read with output?
  // TODO truncate with output?
  // TODO append with output?

  @Test @Ignore
  def outputStream_adds_default_write {
    repeat {
      // no exception? good!
      ops.outputStream().writeString("hi")
    }
  }

  @Test //@Ignore
  def outputStream_can_be_append {
    repeat {
      val fops = ops
      fops.outputStream(APPEND).writeString("more")
      assertEquals(demoData+"more", fops.slurpString)
    }
  }
  
  @Test //@Ignore
  def outputStream_with_read_will_add_write {
    repeat {
      val fops = ops
      fops.outputStream(READ).writeString("more")
      assertEquals("more", fops.slurpString)
    }
  }
  
  @Test //@Ignore
  def truncate_deletes_previous_file {
    repeat {
      val fops = ops
      fops.outputStream(TRUNCATE,WRITE)
      assertEquals("", fops.slurpString)
    }
  }
  
  @Test //@Ignore
  def delete_on_close_deletes_after_operation {
    repeat {
      val fops = ops
      fops.outputStream(DELETE_ON_CLOSE).writeString("hello")
      intercept[IOException] {
        // file should have been deleted so this should throw exception
        fops.slurpString
      }
    }
  }

  @Test @Ignore
  def default_channel_open_options_are_read_write {
    repeat {
      // no exception? good!
      ops.channel().bytesAsInts.take(2).force
      ops.channel().writeString("hi")
    }
  }

  @Test @Ignore
  def default_filechannel_open_options_are_read_write {
    repeat {
      for {resource <- ops.fileChannel() } {
        // no exception? good!
        resource.bytesAsInts.take(2).force
        resource.writeString("hi")
      }
    }
  }
  
  
  @Test @Ignore
  def default_simple_write_also_has_read {
    repeat {
      ops.channel().bytesAsInts.take(2).force
      ops.channel().writeString("hi")
    }
  }
  
  
  @Test @Ignore
  def default_simple_write_will_not_truncate {
    repeat {
      val before = ops.channel().bytesAsInts.size
      ops.channel().write(List(1,2,3) map {_.toByte})
      assertEquals(before, ops.channel().bytesAsInts.size)
    }
  }

}