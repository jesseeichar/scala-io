package scalaio.test

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import language.postfixOps
import scalax.io._
import Line.Terminators._

import org.junit.Assert._
import org.junit.{
Test, Ignore
}

import Constants.TEXT_VALUE

abstract class AbstractReadCharsTests extends scalax.test.sugar.AssertionSugar {

  abstract class Type(val sep: String)

  case object TextNewLine extends Type(NewLine.sep)

  case object TextPair extends Type(RNPair.sep)

  case object TextCarriageReturn extends Type(CarriageReturn.sep)

  case class TextCustom(s: String) extends Type(s)

  protected def readChars(t: Type): ReadChars

  protected def sizeIsDefined = true

  @Test(timeout = 3000)
  def read_all_chars(): Unit = {
    val read = readChars(TextNewLine).chars.toArray
    val expected = TEXT_VALUE.toArray

    assertArrayEquals("expected " + expected.mkString + " but got " + read.mkString, expected, read)
  }

  @Test(timeout = 3000)
  def read_a_subset_of_chars() = {
    val read = readChars(TextNewLine).chars.slice(4, 2).toArray
    val expected = {
      TEXT_VALUE slice (4, 4) toArray
    }

    assertArrayEquals("expected " + expected.mkString + " but got " + read.mkString, expected, read)
  }

  @Test(timeout = 3000)
  def read_all_chars_into_String(): Unit = {
    val read = readChars(TextNewLine).string
    val expected = TEXT_VALUE

    assertEquals(expected, read)
  }

  @Test //(timeout = 3000)
  def read_all_lines_Auto: Unit = {
    testLines("NewLine", TextNewLine, Auto, false)
    testLines("Pair", TextPair, Auto, false)
    testLines("CarriageReturn", TextCarriageReturn, Auto, false)

    testLines("include NewLine", TextNewLine, Auto, true)
    testLines("include Pair", TextPair, Auto, true)
    testLines("include CarriageReturn", TextCarriageReturn, Auto, true)
  }

  @Test(timeout = 3000)
  def read_all_lines(): Unit = {
    testLines("NewLine", TextNewLine, NewLine, false)
    testLines("Pair", TextPair, RNPair, false)
    testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, false)
    testLines("Custom", TextCustom("x"), Custom("x"), false)
  }


  @Test(timeout = 3000)
  def read_all_lines_includeTerminator(): Unit = {
    testLines("Auto", TextNewLine, Auto, true)
    testLines("NewLine", TextNewLine, NewLine, true)
    testLines("Pair", TextPair, RNPair, true)
    testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, true)
    testLines("Custom", TextCustom("x"), Custom("x"), true)
  }

  def testLines(msg: String, t: Type, terminator: Terminator, include: Boolean) {
    val read = readChars(t).lines(terminator, include).toList
    val expected = {
      val data = TEXT_VALUE
      val term = "\n"
      val lines = data.split(term).toList

      val withLastEl =
        if (data.matches("(?ms).*\\s+" + term) || data.matches("(?ms)\\s*" + term)) lines :+ ""
        else lines

      if (include) withLastEl.map {
        _ + t.sep
      }
      else withLastEl


    }
    assertEquals(msg, expected, read)
  }

  @Test(timeout = 3000)
  def read_some_lines(): Unit = {
    val read = readChars(TextNewLine).lines().drop(2).take(2).toList
    val expected = TEXT_VALUE.split("\n").toList.drop(2).take(2)
    assertEquals(expected, read)
  }


}
