/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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

abstract class AbstractSeekableTests extends scalax.test.sugar.AssertionSugar {
    implicit val codec = Codec.UTF8

    /**
     * Seekable containing TEXT_VALUE
     */
    def open(data : Option[String] = None) : Seekable

    val patchParams = 
        ("replaced is MaxValue", 2,"ア",Some(Int.MaxValue)) ::
        ("too large max", 2,"ア",Some(8)) ::
        ("basic", 2,"ア",None) ::
        ("insert", 2,"ア",Some(-1)) ::
        ("start at 0", 0,"ア",None) ::
        ("to large position", 199,"ア",None) ::
        ("very large patch", 2,(1 to 100 mkString ""),None) ::
        ("0 length", 2,"ア",Some(0)) ::
        ("partial overwite",  2,"its a long one!",Some(3)) ::
        Nil

    @Test //@Ignore
    def patchString() : Unit = {

        val testFunction = Function tupled testPatchString _
        patchParams foreach testFunction

        intercept[IllegalArgumentException] {
            open().patchString(-1, "@", 3)
        }
    // test UTF16?
    }
    @Test //@Ignore
    def patchStringASCII() : Unit = {
        val seekable = open(Some("abc"))

        seekable.patchString(1,"x",1)(Codec.ISO8859)

        assertEquals("axc", seekable.slurpString)
    }

    private def testPatchString(msg:String, from:Int, data:String, length : Option[Int]) = {
        val seekable = open()


        val expected = length match {
          case Some(length) => TEXT_VALUE.toList.patch (from.min(Int.MaxValue).toInt, data, length).mkString
          case None => TEXT_VALUE.toList.patch (from.min(Int.MaxValue).toInt, data, length.size).mkString
        }

        length match {
          case Some(length) => seekable.patchString(from,data,length)
          case None => seekable.patchString(from,data)
        }
/*        println("before:   "+(TEXT_VALUE mkString ",").replaceAll("\n","\\\\n"))
        println("patch:    "+(data mkString ",").replaceAll("\n","\\\\n"))
        println("actual:   "+(seekable.slurpString mkString ",").replaceAll("\n","\\\\n"))
        println("expected: "+(expected mkString ",").replaceAll("\n","\\\\n"))
*/
        assertEquals(msg, expected, seekable.slurpString)
    }

    @Test //@Ignore
    def patch() : Unit = {
        val testFunction = Function tupled testPatch _
        patchParams foreach testFunction

        intercept[IllegalArgumentException] {
            open().patch(-1, "@".getBytes(UTF8.name), 3)
        }
    }


    private def testPatch(msg:String, fromInChars:Int, dataString:String, lengthInChars : Option[Int]) = {

        val MAX_VALUE = Int.MaxValue

        val from = TEXT_VALUE.take(fromInChars).getBytes(UTF8.name).size
        val length : Option[Long] = lengthInChars match {
            case Some(MAX_VALUE) => Some(Long.MaxValue)
            case Some(-1) => Some(-1L)
            case Some(lengthInChars) => Some(TEXT_VALUE.slice(fromInChars, fromInChars + lengthInChars).getBytes(UTF8.name).size toLong)
            case None => None
        }

        val data = dataString.getBytes(UTF8.name)

        def test[T <% Traversable[Byte]](datatype: String, dataTransform : Array[Byte] => T) = {
            val seekable = open()
            assertEquals(TEXT_VALUE, seekable.slurpString)

            val expected = lengthInChars match {
                case None => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, data.size)
                case Some(lengthInChars) if(lengthInChars == MAX_VALUE) => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, MAX_VALUE)
                case Some(lengthInChars) => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, length.get.toInt)
            }

            val bytes = dataTransform(data)
            length match {
              case Some(length) => seekable.patch(from,bytes,length)
              case None => seekable.patch(from,bytes)
            }


            assertEquals(datatype+" - patch: '"+msg+"'", expected mkString ",", seekable.byteArray mkString ",")
        }

        test("array", a => a)
        test("list", a => a.toList)
        test("stream", a => a.toStream)

    }

    @Test //@Ignore
    def appendString : Unit = {
        val data = "ア"
        val seekable = open()
        val expected = TEXT_VALUE + data
        seekable.append(data)
        assertEquals(expected, seekable.slurpString)
    }

    @Test //@Ignore
    def appendStrings : Unit = {
      val data = "ア" :: "%" :: "~µ" :: Nil

      def test(sep:String) = {
        val seekable = open()
        val expected = TEXT_VALUE + (data mkString sep)
        seekable.appendStrings(data, sep)
        assertEquals(expected, seekable.slurpString)
      }

      val seekable = open()
      val expected = TEXT_VALUE + (data mkString "")
      seekable.appendStrings(data)
      assertEquals(expected, seekable.slurpString)

      test("123")
      test(",")
    }

    @Test //@Ignore
    def chopString : Unit = {
      val seekable = open()
      val expected = TEXT_VALUE take 2
      seekable.chopString(2)
      assertEquals(expected, seekable.slurpString)
    }

    @Test //@Ignore
    def chop : Unit = {
      val seekable = open()
      val expected = TEXT_VALUE take 2
      seekable.chop(UTF8 encode expected size)
      assertEquals(expected, seekable.slurpString)
    }

    @Test //@Ignore
    def append : Unit = {
        def test(list : Boolean) = {
            val data : String = "ア"
            val seekable = open()
            val expected = TEXT_VALUE + data
            if(list) {
                val bytes = data.getBytes(UTF8.name).toList
                seekable.append(bytes,bytes.size)
            } else {
                val bytes = data.getBytes(UTF8.name)
                seekable.append(bytes,bytes.size)
            }
            assertEquals("append "+(if(list)"list" else "array"), expected, seekable.slurpString)
        }
        test(false)
        test(true)
    }
}