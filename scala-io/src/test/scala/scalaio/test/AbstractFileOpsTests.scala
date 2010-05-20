/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._

import org.junit.Assert._
import org.junit.{
  Test, Ignore
}

import java.io.IOException

trait AbstractFileOpsTests extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8

  /**
   * create a file ops object on a path that is an existing file.  File must be at minimum 5 characters long
   */
  def ops(implicit data : Array[Byte] = "hello this is demo data".getBytes(codec.name)) : FileOps

  // TODO defaults output
  // TODO read with output?
  // TODO truncate with output?
  // TODO append with output?

  @Test //@Ignore
  def default_channel_open_options_are_read_write {
    repeat {
      // no exception? good!
      ops.channel().bytesAsInts.take(2).force
      ops.channel().writeString("hi")
    }
  }

  @Test //@Ignore
  def default_filechannel_open_options_are_read_write {
    repeat {
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