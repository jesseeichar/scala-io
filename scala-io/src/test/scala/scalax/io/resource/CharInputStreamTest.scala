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
    IOException, StringReader, InputStreamReader, BufferedReader
}

class CharInputStreamTest extends AssertionSugar with IOSugar{
    implicit val codec = Codec("UTF-16")
 
    val sample ="a1?£©àäカ゚ゼ"

    @Test
    def write_bytes_to_writer() : Unit = {
        val reader = new StringReader(sample)
        val in = new CharInputStream(Left(reader))
        
        val in2 = new BufferedReader(new InputStreamReader(in, codec.name))
        assertEquals(sample, in2.readLine)
    }
}