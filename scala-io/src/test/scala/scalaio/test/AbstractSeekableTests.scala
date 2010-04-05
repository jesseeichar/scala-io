/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.io.resource.Seekable
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

    def open() : Seekable

    @Test
    def patchString() : Unit = {
        testPatchString(2,"@",Long.MaxValue)
        intercept[IndexOutOfBoundsException]{
            testPatchString(199,"@",Long.MaxValue)
        }
        testPatchString(2,1 to 100 mkString "",Long.MaxValue)
        testPatchString(2,"@",0)
        testPatchString(2,"its a long one!",3)
        
    // test UTF16?        
    }
    
    private def testPatchString(from:Long, data:String, length : Long) = {
        val seekable = open()
        val expected = {TEXT_VALUE patch (from.min(Int.MaxValue).toInt, data, length.min(Int.MaxValue).toInt ) toArray}
        seekable.patchString(from,data,length)
        assertEquals(expected, seekable.slurpString)
    }

}