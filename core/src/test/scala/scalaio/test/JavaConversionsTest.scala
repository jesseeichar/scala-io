package scalaio.test

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import scalax.io._
import JavaConverters._
import org.junit.Assert._
import org.junit.Test

class JavaConversionsTest extends scalax.test.sugar.AssertionSugar {

  @Test
  def traversable_asInput {
    assertEquals((1 to 10).toList, (1 to 10).map(_.toByte).asInput.bytesAsInts.toList)
    assertEquals((1 to 10).toList, (1 to 10).map(_.toByte).toList.asInput.bytesAsInts.toList)
    assertEquals((1 to 10).toList, (1 to 10).asInput.bytesAsInts.toList)

    val s = "hello world"
    assertEquals(s, s.getBytes("UTF-8").asInput.chars.mkString)

    val processor = for {
      inp <- (1 to 10).asInput.bytesAsInts.processor
      _ <- inp.repeatUntilEmpty()
      next <- inp.next
    } yield next.toString

    assertEquals((1 to 10).map(_.toString).toList, processor.traversable.toList)
  }

}
