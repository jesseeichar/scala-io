/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import Codec.UTF8
import Line.Terminators._

import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test,Ignore
}

import java.io.IOException
import Constants.TEXT_VALUE

abstract class AbstractInputTests extends scalax.test.sugar.AssertionSugar {
    sealed trait Type
    case object Image extends Type
    abstract class Text(val sep:String) extends Type
    case object TextNewLine extends Text(NewLine.sep)
    case object TextPair extends Text(Pair.sep)
    case object TextCarriageReturn extends Text(CarriageReturn.sep)
    case class  TextCustom(s:String) extends Text(s)

    protected def input(t:Type) : Input
    protected def sizeIsDefined = true

    @Test(timeout = 3000)
    def provide_length_for_files() : Unit = {
        val size = input(Image).size
        if(sizeIsDefined) {
          assertTrue(size.isDefined)
          assertEquals(Constants.IMAGE_FILE_SIZE, size.get)
        } else {
          assertTrue(size.isEmpty)
        }
    }

    @Test(timeout = 3000)
    def read_all_bytes() : Unit = {
        val bytes = input(TextNewLine).bytes.toArray

        val expected = TEXT_VALUE getBytes  UTF8.name
        val bytesString = new String(bytes, UTF8.name)

        assertEquals(expected.size, bytes.size)
        assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }

    @Test(timeout = 3000)
    def read_a_subset_of_bytes() = {
        val bytes = input(TextNewLine).bytes.slice(4,4).toArray

        val expected = TEXT_VALUE getBytes UTF8.name slice (4,4)
        val bytesString = new String(bytes, UTF8.name)

        assertEquals(expected.size, bytes.size)
        assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }


    @Test(timeout = 3000)
    def read_all_bytes_as_Ints() : Unit = {
        val ints = input(TextNewLine).bytesAsInts.toArray
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


    @Test(timeout = 3000)
    def read_all_bytes_into_array() : Unit = {
      val bytes = input(TextNewLine).byteArray

      val expected = TEXT_VALUE getBytes  UTF8.name
      val bytesString = new String(bytes, UTF8.name)

      assertEquals(expected.size, bytes.size)
      assertArrayEquals("expected '"+TEXT_VALUE+"' but got '"+bytesString+"'", 
                 expected, bytes)
    }
    
    // byte ops done now chars
    
    @Test(timeout = 3000)
    def read_all_chars() : Unit = {
        val read = input(TextNewLine).chars(UTF8).toArray

        val expected = TEXT_VALUE.toArray

        assertArrayEquals("expected "+expected.mkString +" but got "+read.mkString, expected, read)
    }

    @Test(timeout = 3000)
    def read_a_subset_of_chars() = {
        val read = input(TextNewLine).chars(UTF8).slice(4,2).toArray

        val expected = {TEXT_VALUE slice (4,4) toArray}

        assertArrayEquals("expected "+expected.mkString +" but got "+read.mkString,expected, read)
    }

    @Test(timeout = 3000)
    def read_all_chars_into_String() : Unit = {
      val read = input(TextNewLine).slurpString(UTF8)

      val expected = TEXT_VALUE
      
      assertEquals(expected, read)
    }

    @Test(timeout = 3000)
    def read_all_lines_auto() : Unit = {
        testLines("NewLine", TextNewLine, Auto(), false)
        testLines("Pair", TextPair, Auto(), false)
        testLines("CarriageReturn", TextCarriageReturn, Auto(), false)

        testLines("include NewLine", TextNewLine, Auto(), true)
        testLines("include Pair", TextPair, Auto(), true)
        testLines("include CarriageReturn", TextCarriageReturn, Auto(), true)
    }

    @Test(timeout = 3000)
    def read_all_lines() : Unit = {
        testLines("NewLine", TextNewLine, NewLine, false)
        testLines("Pair", TextPair, Pair, false)
        testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, false)
        testLines("Custom", TextCustom("x"), Custom("x"), false)
    }
    
    
    @Test(timeout = 3000)
    def read_all_lines_includeTerminator() : Unit = {
        testLines("Auto", TextNewLine, Auto(), true)
        testLines("NewLine", TextNewLine, NewLine, true)
        testLines("Pair", TextPair, Pair, true)
        testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, true)
        testLines("Custom", TextCustom("x"), Custom("x"), true)
    }
    
    def testLines(msg:String, t:Text, terminator:Terminator, include:Boolean) {
        val read = input(t).lines(terminator, include)(UTF8).toList
        val expected = {
                val lines = TEXT_VALUE.split("\n").toList
                if(include) lines.map{_ + t.sep}
                else lines
            }
        assertEquals(msg, expected, read)
    }
    
    @Test(timeout = 3000)
    def read_some_lines() : Unit = {
        val read = input(TextNewLine).lines()(UTF8).drop(2).take(2).toList
        val expected = TEXT_VALUE.split("\n").toList.drop(2).take(2)
        assertEquals(expected, read)
    }


}