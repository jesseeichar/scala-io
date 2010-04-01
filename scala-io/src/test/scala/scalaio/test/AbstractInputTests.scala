/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import Codec._

import Path.AccessModes._
import Line.Terminators._

import org.junit.Assert._
import org.junit.{
  Test,Ignore
}

import java.io.IOException
import Constants.TEXT_VALUE

abstract class AbstractInputTests extends scalax.test.sugar.AssertionSugar {

    sealed trait Type
    case object Image extends Type
    case object Text extends Type

    protected def input(t:Type) : Input
    protected def sizeIsDefined = true

    @Test
    def provide_length_for_files() : Unit = {
        val size = input(Image).size
        if(sizeIsDefined) {
          assertTrue(size.isDefined)
          assertEquals(Constants.IMAGE_FILE_SIZE, size.get)
        } else {
          assertTrue(size.isEmpty)
        }
    }

    @Test
    def read_all_bytes() : Unit = {
        val bytes = input(Text).bytes.toArray

        val expected = TEXT_VALUE getBytes  "UTF-8"
        val bytesString = new String(bytes, "UTF-8")

        assertEquals(expected.size, bytes.size)
        assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }

    @Test
    def read_a_subset_of_bytes() = {
        val bytes = input(Text).bytes.slice(4,4).toArray

        val expected = TEXT_VALUE getBytes "UTF-8" slice (4,4)
        val bytesString = new String(bytes, "UTF-8")

        assertEquals(expected.size, bytes.size)
        assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }


    @Test
    def read_all_bytes_as_Ints() : Unit = {
        val ints = input(Text).bytesAsInts.toArray

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
      val bytes = input(Text).slurpBytes

      val expected = TEXT_VALUE getBytes  "UTF-8"
      val bytesString = new String(bytes, "UTF-8")

      assertEquals(expected.size, bytes.size)
      assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }
    
    // byte ops done now chars
    
    @Test
    def read_all_chars() : Unit = {
        val read = input(Text).chars(UTF8).toArray

        val expected = TEXT_VALUE.toArray

        assertArrayEquals("expected "+expected.mkString +" but got "+read.mkString, expected, read)
    }

    @Test
    def read_a_subset_of_chars() = {
        val read = input(Text).chars(UTF8).slice(4,2).toArray

        val expected = {TEXT_VALUE slice (4,4) toArray}

        assertArrayEquals("expected "+expected.mkString +" but got "+read.mkString,expected, read)
    }

    @Test
    def read_all_chars_into_String() : Unit = {
      val read = input(Text).slurpString(UTF8)

      val expected = TEXT_VALUE
      
      assertEquals(expected, read)
    }

    @Test
    def read_all_lines_auto() : Unit = {
      val read = input(Text).lines()(UTF8).toList
      val expected = TEXT_VALUE.split("\n").toList
      
      assertEquals(expected, read)
    }

    @Test
    def read_all_lines_NewLine() : Unit = {
        val read = input(Text).lines(terminator = NewLine)(UTF8).toList
        val expected = TEXT_VALUE.split("\n").toList

        assertEquals(expected, read)
    }

    @Test
    def read_all_lines_CarriageReturn() : Unit = {
        fail("not implemented")
    }

    @Test
    def read_all_lines_Pair() : Unit = {
        fail("not implemented")
    }

    @Test
    def read_all_lines_Custom() : Unit = {
        fail("not implemented")
    }
    
    
    @Test
    def read_all_lines_includeTerminator() : Unit = {
        fail("not implemented")
    }
    
    @Test
    def read_some_lines() : Unit = {
        fail("not implemented")
    }


}