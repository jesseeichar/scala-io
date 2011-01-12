/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import java.io.{
    StringReader, BufferedReader
}

class StreamIteratorTest extends AssertionSugar with IOSugar{
    implicit val codec = Codec("UTF-8")

    val sample ="a1?£©àäカ゚ゼ"

    @Test
    def stream_converts_chars_to_bytes() : Unit = {
        def reader = new BufferedReader(new StringReader(sample))

        assertEquals(sample, reader.readLine)

        assertEquals(sample, StreamIterator(reader) mkString "")
    }

    @Test
    def convert_codec() : Unit = {
        sample foreach {ch =>
            val c = ch.toString

            val reader = new StringReader(c)
            val iter = StreamIterator(reader, codec, Codec("UTF-16"))

            val converted : Char = iter.next
            assertFalse(iter.hasNext)
            assertEquals(new String(c.getBytes(codec.name), "UTF-16").head, converted)
        }
    }

}
