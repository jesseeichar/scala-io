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
import Constants.TEXT_VALUE

abstract class AbstractReadBytesT extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8

  sealed trait Type
  case object Image extends Type
  case object Text extends Type

  protected def readBytes(t:Type) : ReadBytes

  @Test
  def provide_length_for_files() : Unit = {
      val size = readBytes(Image).size
      assertTrue(size.isDefined)
      assertEquals(Constants.IMAGE_FILE_SIZE, size.get)
  }
  
  @Test
  def read_all_bytes() : Unit = {
      val bytes = readBytes(Text).bytes.toArray

      val expected = TEXT_VALUE getBytes  "UTF-8"
      val bytesString = new String(bytes, "UTF-8")

      assertEquals(expected.size, bytes.size)
      assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
  }

  @Test
  def read_a_subset_of_bytes() = {
      val bytes = readBytes(Text).bytes.slice(4,4).toArray

      val expected = TEXT_VALUE getBytes "UTF-8" slice (4,4)
      val bytesString = new String(bytes, "UTF-8")

      assertEquals(expected.size, bytes.size)
      assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
  }

  
  @Test
  def read_all_bytes_as_Ints() : Unit = {
      val ints = readBytes(Text).bytesAsInts.toArray

      val expected = {
          val in = Constants.TEXT.openStream
          try {
              var i = in.read()
              val buffer = new collection.mutable.ArrayBuffer[Int]()
              while(i != -1) {
                  buffer += i
                  i = in.read()
              }
              buffer.toArray
          } finally {
              in.close
          }
      }

      assertEquals(expected.size, ints.size)
      assertArrayEquals(expected, ints)
  }
  
  
  @Test
  def read_all_bytes_into_array() : Unit = {
      val bytes = readBytes(Text).slurpBytes()

      val expected = TEXT_VALUE getBytes  "UTF-8"
      val bytesString = new String(bytes, "UTF-8")

      assertEquals(expected.size, bytes.size)
      assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
  }
  

}