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

abstract class AbstractSeekableTests extends scalax.test.sugar.AssertionSugar {
    implicit val codec = Codec.UTF8

    /**
     * Seekable containing TEXT_VALUE
     */
    def open() : Seekable

    val patchParams = 
        ("replaced is MaxValue", 2,"ア",Int.MaxValue) ::
        ("too large max", 2,"ア",8) ::
        ("basic", 2,"ア",-2) ::
        ("insert", 2,"ア",-1) ::
        ("start at 0", 0,"ア",-2) ::
        ("to large position", 199,"ア",-2) ::
        ("very large patch", 2,(1 to 100 mkString ""),-2) ::
        ("0 length", 2,"ア",0) ::
        ("use only part of data", 2,"its a long one!",3) ::
        Nil

    @Test @Ignore
    def patchString() : Unit = {
        val testFunction = Function tupled testPatchString _
        patchParams foreach testFunction
        
        intercept[IllegalArgumentException] {
            open.patchString(-1, "@", 3)
        }
    // test UTF16?        
    }
    
    private def testPatchString(msg:String, from:Int, data:String, length : Int) = {
        val seekable = open()
        val expected = TEXT_VALUE patch (from.min(Int.MaxValue).toInt, data, length.min(Int.MaxValue).toInt )
        seekable.patchString(from,data,length)
        assertEquals(msg, expected, seekable.slurpString)
    }

    @Test
    def patch() : Unit = {
        val testFunction = Function tupled testPatch _
        patchParams foreach testFunction 

        intercept[IllegalArgumentException] {
            open.patch(-1, "@".getBytes(UTF8.name), 3)
        }
    }
    
    
    private def testPatch(msg:String, fromInChars:Int, dataString:String, lengthInChars : Int) = {
        System.err.println("starting '"+msg+"'")


        val MAX_VALUE = Int.MaxValue

        val from = TEXT_VALUE.take(fromInChars).getBytes(UTF8.name).size
        val length = lengthInChars match {
            case MAX_VALUE => Long.MaxValue
            case -1 => -1
            case -2 => -2
            case _ => TEXT_VALUE.slice(fromInChars, fromInChars + lengthInChars).getBytes(UTF8.name).size
        }

        val data = dataString.getBytes(UTF8.name)

        def test[T <% Traversable[Byte]](datatype: String, dataTransform : Array[Byte] => T) = {
            val seekable = open()
            assertEquals(TEXT_VALUE, seekable.slurpString)

            val expected = lengthInChars match {
                case -2 => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, data.size)
                case _ if(lengthInChars == MAX_VALUE) => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, MAX_VALUE)
                case _ => TEXT_VALUE.getBytes(UTF8.name)  patch (from, data, length.toInt)
            }
            
            val bytes = dataTransform(data)
            seekable.patch(from,bytes,length)
                
            System.err.println("before:   "+(TEXT_VALUE.getBytes(UTF8.name) mkString ","))
            System.err.println("actual:   "+(seekable.byteArray mkString ","))
            System.err.println("expected: "+(expected mkString ","))
            assertEquals(datatype+" - patch: '"+msg+"'", expected mkString ",", seekable.byteArray mkString ",")
        }
        
        test("array", a => a)
        test("list", a => a.toList)
        test("stream", a => a.toStream)
        
        System.err.println("done '"+msg+"'")
        System.err.println()
        System.err.println()
        
    }

    @Test  @Ignore
    def appendString : Unit = {
        val data = "ア"
        val seekable = open()
        val expected = TEXT_VALUE + data
        seekable.appendString(data)
        assertEquals(expected, seekable.slurpString)
    }
    
    @Test @Ignore
    def appendStrings : Unit = {
        fail("not implemented")
    }

    @Test  @Ignore
    def truncate : Unit = {
        fail("not implemented")
    }

    @Test  @Ignore
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