/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io._
import scalax.io.resource._
import Path.AccessModes._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.{
    IOException, StringWriter
}

class WriterOutputStreamTest extends AssertionSugar with IOSugar{
    implicit val codec = Codec("UTF-8")
     
    val sample ="a1?£©àäカ゚ゼ"

    @Test @Ignore
    def write_bytes_as_ints_to_writer() : Unit = {
        
        val writer = new StringWriter()
        val out = new WriterOutputStream(writer)
        
        sample.getBytes(codec.name) foreach {b => out.write(b.toInt)}
        
        assertEquals(sample, writer.toString)
    }
    
    @Test
    def write_byte_array_to_writer() : Unit = {
        
        val writer = new StringWriter()
        val out = new WriterOutputStream(writer)
        
        out.write(sample.getBytes(codec.name))
        
        assertEquals(sample, writer.toString)
    }
    
    
    @Test
    def write_sub_bytes_array_to_writer() : Unit = {
        
        val writer = new StringWriter()
        val out = new WriterOutputStream(writer)
        val bytes = sample.getBytes(codec.name)
        out.write(bytes, 0 , bytes.length)
        
        assertEquals(sample, writer.toString)
    }

}