/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import org.junit.Assert._
import org.junit.{
Test, Ignore
}

import Constants.TEXT_VALUE

abstract class AbstractOutputTests extends scalax.test.sugar.AssertionSugar {
  private final val DEFAULT_DATA = "default data"

  def open(): (Input, Output)

  @Test //@Ignore
  def write_bytes(): Unit = {
    val (input, output) = open()
    val bytes = DEFAULT_DATA.getBytes

    output write bytes

    assertArrayEquals(bytes, input.byteArray)
  }

  @Test //@Ignore
  def write_string(): Unit = {
    implicit val codec = Codec.UTF8

    val (input, output) = open()

    output write DEFAULT_DATA

    assertEquals(DEFAULT_DATA, input.slurpString)
  }

  @Test //@Ignore
  def write_charseq(): Unit = {
    implicit val codec = Codec.UTF8

    val (input, output) = open()
    val charSeq = new StringBuilder(DEFAULT_DATA)

    output writeChars charSeq

    assertEquals(DEFAULT_DATA, input.slurpString)
  }

  @Test //@Ignore
  def write_traversable_char(): Unit = {
    implicit val codec = Codec.UTF8

    val (input, output) = open()

    output writeChars DEFAULT_DATA.toList

    assertEquals(DEFAULT_DATA, input.slurpString)
  }


  @Test //@Ignore
  def write_many_strings(): Unit = {
    implicit val codec = Codec.UTF8

    val (input, output) = open()

    output writeStrings (DEFAULT_DATA :: DEFAULT_DATA :: DEFAULT_DATA :: Nil)
    assertEquals(DEFAULT_DATA + DEFAULT_DATA + DEFAULT_DATA, input.slurpString)

    val (input2, output2) = open()

    output2 writeStrings (DEFAULT_DATA :: DEFAULT_DATA :: DEFAULT_DATA :: Nil, "-")
    assertEquals(DEFAULT_DATA + "-" + DEFAULT_DATA + "-" + DEFAULT_DATA, input2.slurpString)
  }


}
