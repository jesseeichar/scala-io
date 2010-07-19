/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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
   * create a path object to an existing file.  File must be at minimum 5 characters long
   */
  def path(implicit data : Array[Byte] = demoData.getBytes(codec.name)) : Path

  // TODO read with channel/fileChannel?
  // TODO truncate with channel/fileChannel?
  // TODO append with channel/fileChannel?

  @Test //@Ignore
  def open_can_perform_several_ops {
    path.ops.open() {stream =>
      assertEquals("initial read works", demoData take 2, stream.chars take 2 mkString)
      stream.chop(2)
      assertEquals("chop works", demoData take 2, stream.slurpString)
      stream.appendString(demoData drop 2)
      assertEquals("append works",demoData, stream.slurpString)
    }
  }
  
  @Test //@Ignore
  def lock_provides_normal_functionality {
    path.ops.open() {stream =>
      assertEquals("initial read works", demoData take 2, stream.chars take 2 mkString)
      stream.chop(2)
      assertEquals("chop works", demoData take 2, stream.slurpString)
      stream.appendString(demoData drop 2)
      assertEquals("append works",demoData, stream.slurpString)
    }
  }

  @Test //@Ignore
  def outputStream_adds_default_write {
    repeat {
      val ops = path.ops
      // no exception? good!
      ops.outputStream().writeString("hi")
    }
  }

  @Test //@Ignore
  def outputStream_can_be_append {
    repeat {
      val ops = path.ops
      ops.outputStream(APPEND).writeString("more")
      assertEquals(demoData+"more", ops.slurpString)
    }
  }
  
  @Test //@Ignore
  def outputStream_with_read_will_add_write {
    repeat {
      val ops = path.ops
      ops.outputStream(Read).writeString("more")
      assertEquals("more", ops.slurpString)
    }
  }
  
  @Test //@Ignore
  def truncate_deletes_previous_file {
    repeat {
      val ops = path.ops
      ops.outputStream(TRUNCATE,Write)
      assertEquals("", ops.slurpString)
    }
  }
  
  @Test //@Ignore
  def create_new_fails_when_file_exists {
    repeat {
      val ops = path.ops
      intercept[IOException] {
        ops.outputStream(CREATE_NEW)
      }
      assertEquals(demoData, ops.slurpString)
    }
  }
  
  @Test //@Ignore
  def delete_on_close_deletes_after_operation {
    implicit val times = 1
    repeat {
      val ops = path.ops
      ops.outputStream(DELETE_ON_CLOSE).writeString("hello")
      intercept[IOException] {
        // file should have been deleted so this should throw exception
        ops.slurpString
      }
    }
  }

  @Test //@Ignore
  def default_channel_open_options_are_read_write {
    repeat {
      val ops = path.ops
      // no exception? good!
      ops.channel().bytesAsInts.take(2).force
      ops.channel().writeString("hi")
    }
  }

  @Test //@Ignore
  def openning_channel_supports_several_openOptions {
    repeat {
      val ops = path.ops
      // Need to have mixed orders so that we can be sure the sorting takes place 
      ops.channel(DSYNC,Write,Write,TRUNCATE,SYNC,CREATE,Read,Read).bytesAsInts.take(2).force
      // wish there was a way to test channel...
    }
  }
  @Test //@Ignore
  def default_filechannel_open_options_are_read_write {
    repeat {
      val ops = path.ops
      for {resource <- ops.fileChannel() } {
        // no exception? good!
        resource.bytesAsInts.take(2).force
        resource.writeString("hi")
      }
    }
  }
  
  @Test //@Ignore
  def default_simple_write_also_has_read {
    repeat {
      val ops = path.ops
      ops.channel().bytesAsInts.take(2).force
      ops.channel().writeString("hi")
    }
  }
  
  @Test //@Ignore  // Dead locks
  def default_simple_write_will_not_truncate {
    repeat {
      val ops = path.ops
      val before = ops.channel().bytesAsInts.size
      ops.channel().write(List(1,2,3) map {_.toByte})
      assertEquals(before, ops.channel().bytesAsInts.size)
    }
  }
  
  
  /*  Removed from API since it may be difficult or even impossible on
      non-native file systems
  @Test //@Ignore
  def execute_throws_exception_on_non_existant_file {
    val p = path("echo hi".getBytes)
    p.delete()
    intercept[IOException] {
      p.ops.execute()
    }
  }
  
  @Test //@Ignore
  def execute_throws_exception_on_non_executable_file {
    val p = path("echo hi".getBytes)
    p.access = "-x"
    intercept[IOException] {
      p.ops.execute()
    }
  }

  // TODO Execute
  */
}