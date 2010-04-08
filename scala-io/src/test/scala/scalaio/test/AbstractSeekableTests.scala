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

    @Test @Ignore
    def patchString() : Unit = {
        testPatch("too large max", 2,"ア",Long.MaxValue)
        testPatch("basic", 2,"ア",-1)
        testPatch("to large position", 199,"ア",-1)
        testPatch("very large patch", 2,(1 to 100 mkString ""),-1)
        testPatch("0 length", 2,"ア",0)
        testPatch("use only part of data", 2,"its a long one!",3)
        
        intercept[IllegalArgumentException] {
            open.patchString(-1, "@", 3)
        }
    // test UTF16?        
    }
    
    private def testPatchString(msg:String, from:Long, data:String, length : Long) = {
        val seekable = open()
        val expected = TEXT_VALUE patch (from.min(Int.MaxValue).toInt, data, length.min(Int.MaxValue).toInt )
        seekable.patchString(from,data,length)
        assertEquals(msg, expected, seekable.slurpString)
    }

    @Test
    def patch() : Unit = {
        testPatch("too large max", 2,"ア",Long.MaxValue)
        testPatch("basic", 2,"ア",-1)
        testPatch("to large position", 199,"ア",-1)
        testPatch("very large patch", 2,(1 to 100 mkString ""),-1)
        testPatch("0 length", 2,"ア",0)
        testPatch("use only part of data", 2,"its a long one!",3)

        intercept[IllegalArgumentException] {
            open.patch(-1, "@".getBytes(UTF8.name), 3)
        }
    }
    
    private def testPatch(msg:String, fromInChars:Int, data:String, length : Long) = {
        val from = TEXT_VALUE.take(fromInChars).getBytes(UTF8.name).size
        def test(list : Boolean) = {
            val seekable = open()
            assertEquals(TEXT_VALUE, seekable.slurpString)
            
            System.err.println(seekable.byteArray mkString ",")
            
            val expected = TEXT_VALUE patch (fromInChars, data.toSeq, length.min(Int.MaxValue).toInt )
            if(list) {
                val bytes = data.getBytes(UTF8.name).toList
                seekable.patch(from,bytes,length)
            } else {
                val bytes = data.getBytes(UTF8.name)
                seekable.patch(from,bytes,length)
            }
            System.err.println(data.getBytes(UTF8.name) mkString ",")
            assertEquals("is list = "+list+" patch: '"+msg+"'", expected.getBytes(UTF8.name) mkString ",", seekable.byteArray mkString ",")
        }
        test(false)
        test(true)
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