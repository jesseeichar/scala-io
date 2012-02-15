/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.io.StandardOpenOption._

import org.junit.Assert._
import org.junit.Test

import java.io.IOException
import scalax.file.Path

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
    path.open(StandardOpenOption.ReadWrite) {stream =>
      assertEquals("initial read works", demoData take 2, stream.chars take 2 mkString)
      stream.truncate(2)
      assertEquals("chop works", demoData take 2, stream.slurpString)
      stream.append(demoData drop 2)
      assertEquals("append works",demoData, stream.slurpString)
    }
  }

  @Test //@Ignore
  def lock_provides_normal_functionality {
    path.open() {stream =>
      assertEquals("initial read works", demoData take 2, stream.chars take 2 mkString)
      stream.truncate(2)
      assertEquals("chop works", demoData take 2, stream.slurpString)
      stream.append(demoData drop 2)
      assertEquals("append works",demoData, stream.slurpString)
    }
  }

  @Test //@Ignore
  def outputStream_adds_default_write {
    repeat {
      // no exception? good!
      path.outputStream().write("hi")
    }
  }

  @Test //@Ignore
  def outputStream_can_be_append {
    repeat {
      val p = path
      p.outputStream(Append).write("more")
      assertEquals(demoData+"more", p.slurpString)
    }
  }

  @Test //@Ignore
  def outputStream_with_read_will_add_write {
    repeat {
      val p = path
      p.outputStream(Read).write("more")
      assertEquals("more", p.slurpString)
    }
  }

  @Test //@Ignore
  def truncate_deletes_previous_file {
    repeat {
      val p = path
      p.outputStream(Truncate,Write).open().close()
      assertEquals("", p.slurpString)
    }
  }

  @Test //@Ignore
  def truncate_takes_precedence_over_Append {
    repeat {
      val p = path
      p.outputStream(Truncate,Write,Append).open().close()
      assertEquals("", p.slurpString)
    }
  }

  @Test //@Ignore
  def obtaining_resource_does_not_fail_when_not_opened {
      val p = path
      p.outputStream(CreateNew)
      // no error, that is a pass
      p.channel(CreateNew)
      // no error, that is a pass
      p.delete(true)
      p.inputStream()
      // no error, that is a pass

  }

  @Test //@Ignore
  def create_new_fails_when_file_exists {
      val p = path
      intercept[Exception] {
        p.outputStream(CreateNew).open().close()
      }
      assertEquals(demoData, p.slurpString)
  }

  @Test //@Ignore
  def delete_on_close_deletes_after_operation {
    implicit val times = 1
    repeat {
      val p = path
      p.outputStream(DeleteOnClose).write("hello")
      assertFalse(p.exists)
      intercept[IOException] {
        // file should have been deleted so this should throw exception
        p.slurpString
      }
    }
  }

  @Test //@Ignore
  def default_channel_open_options_are_read_write {
    repeat {
      val p = path
      // no exception? good!
      p.channel().bytesAsInts.take(2).force
      p.channel().write("hi")
    }
  }

  @Test //@Ignore
  def openning_channel_supports_several_openOptions {
    repeat {
      // Need to have mixed orders so that we can be sure the sorting takes place
      path.channel(DSync,Write,Write,Truncate,Sync,Create,Read,Read).bytesAsInts.take(2).force
      // wish there was a way to test channel...
    }
  }
  @Test //@Ignore
  def default_filechannel_open_options_are_read_write {
    repeat {
      val p = path
      for {resource <- p.fileChannel() } {
        // no exception? good!
        resource.bytesAsInts.take(2).force
        resource.write("hi")
      }
    }
  }

  @Test //@Ignore
  def default_simple_write_also_has_read {
    repeat {
      val p = path
      p. channel().bytesAsInts.take(2).force
      p.channel().write("hi")
    }
  }

  @Test //@Ignore
  def default_simple_write_will_not_truncate {
    repeat {
      val p = path
      val before = p.channel().bytesAsInts.size
      p.channel().write(List(1,2,3) map {_.toByte})
      assertEquals(before, p.channel().bytesAsInts.size)
    }
  }

  @Test //@Ignore
  def createFull_open_option_creates_parents {
    repeat {
      val p = path
      p.outputStream(StandardOpenOption.CreateFull, StandardOpenOption.Write).write("data")
      assertTrue(p.exists)
      assertTrue(p.parent.forall{_.exists})
    }
  }

  @Test //@Ignore
  def create_open_option_will_not {
    repeat {
      val p = path \ "child"
      assert (p.nonExistent)
      intercept[IOException] { p.outputStream(StandardOpenOption.Create, StandardOpenOption.Write).write("data") }
    }
  }


  /*  Removed from API since it may be difficult or even impossible on
      non-native file systems
  @Test //@Ignore
  def execute_throws_exception_on_non_existant_file {
    val p = path("echo hi".getBytes)
    p.delete()
    intercept[IOException] {
      p.execute()
    }
  }

  @Test //@Ignore
  def execute_throws_exception_on_non_executable_file {
    val p = path("echo hi".getBytes)
    p.access = "-x"
    intercept[IOException] {
      p.execute()
    }
  }

  // TODO Execute
  */
}
