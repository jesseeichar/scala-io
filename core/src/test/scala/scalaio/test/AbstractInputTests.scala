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
import Codec.UTF8
import Line.Terminators._
import org.junit.Assert._
import org.junit.{
  Test,
  Ignore
}
import Constants.TEXT_VALUE
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import java.nio.channels.Channels
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.IOException
import java.io.Closeable

object AbstractInputTests {

  sealed trait Type

  case object Image extends Type

  abstract class Text(val sep: String) extends Type

  case object TextNewLine extends Text(NewLine.sep)

  case object TextPair extends Text(RNPair.sep)

  case object TextCarriageReturn extends Text(CarriageReturn.sep)

  case class TextCustom(s: String) extends Text(s)

  case class TextCustomData(s: String, data: String) extends Text(s)

  case object ErrorOnRead extends Type {
    def errorInputStream = new InputStream {
      override def read = throw new IOException("error")
      override def read(buf: Array[Byte], off: Int, len: Int) =
        throw new IOException("error")
    }
  }

  case object ErrorOnClose extends Type {
    def errorInputStream = new ByteArrayInputStream("hello".getBytes("UTF-8")) {
      override def close = throw new IOException("error")
    }
  }

}

import AbstractInputTests._

abstract class AbstractInputTests extends scalax.test.sugar.AssertionSugar {

  protected def input(t: Type, openFunction: () => Unit = () => (), closeFunction: () => Unit = () => ()): Input

  protected def sizeIsDefined = true
  protected def canExecuteOpenFunction = true

  @Test(timeout = 3000) //@Ignore
  def provide_length_for_files(): Unit = {
    val size = input(Image).size
    if (sizeIsDefined) {
      assertTrue(size.isDefined)
      assertEquals(Constants.IMAGE_FILE_SIZE, size.get)
    } else {
      assertTrue(size.isEmpty)
    }
  }

  @Test(timeout = 3000) //@Ignore
  def read_all_bytes(): Unit = {
    val bytes = input(TextNewLine).bytes.toArray

    val expected = TEXT_VALUE getBytes UTF8.name
    val bytesString = new String(bytes, UTF8.name)

    assertEquals(expected.size, bytes.size)
    assertArrayEquals("expected '" + TEXT_VALUE + "' but got '" + bytesString + "'",
      expected, bytes)
  }

  @Test(timeout = 3000) //@Ignore
  def read_a_subset_of_bytes() = {
    val bytes = input(TextNewLine).bytes.slice(4, 4).toArray

    val expected = TEXT_VALUE getBytes UTF8.name slice (4, 4)
    val bytesString = new String(bytes, UTF8.name)

    assertEquals(expected.size, bytes.size)
    assertArrayEquals("expected '" + TEXT_VALUE + "' but got '" + bytesString + "'",
      expected, bytes)
  }

  @Test //(timeout = 3000) //@Ignore
  def read_all_bytes_as_Ints(): Unit = {
    val ints = input(TextNewLine).bytesAsInts.toArray
    val expected = {
      val bytes = Constants.TEXT_VALUE.getBytes(Codec.UTF8.charSet)
      val in = new ByteArrayInputStream(bytes)
      try {
        var i = in.read()
        val buffer = new collection.mutable.ArrayBuffer[Int]()
        while (i != -1) {
          buffer += i
          i = in.read()
        }
        buffer.toArray
      } finally {
        in.close
      }
    }

    assertEquals(expected.size, ints.size)
    assertArrayEquals(expected, ints)
  }

  @Test(timeout = 3000) //@Ignore
  def read_all_bytes_into_array(): Unit = {
    val bytes = input(TextNewLine).byteArray

    val expected = TEXT_VALUE getBytes UTF8.name
    val bytesString = new String(bytes, UTF8.name)

    assertEquals(expected.size, bytes.size)
    assertArrayEquals("expected '" + TEXT_VALUE + "' but got '" + bytesString + "'",
      expected, bytes)
  }

  // byte ops done now chars

  @Test(timeout = 3000) //@Ignore
  def read_all_chars(): Unit = {
    val read = input(TextNewLine).chars(UTF8).toArray

    val expected = TEXT_VALUE.toArray

    assertArrayEquals("expected " + expected.mkString + " but got " + read.mkString, expected, read)
  }

  @Test(timeout = 3000) //@Ignore
  def read_a_subset_of_chars() = {
    val read = input(TextNewLine).chars(UTF8).slice(4, 2).toArray

    val expected = {
      TEXT_VALUE slice (4, 4) toArray
    }

    assertArrayEquals("expected " + expected.mkString + " but got " + read.mkString, expected, read)
  }

  @Test(timeout = 3000) //@Ignore
  def read_all_chars_into_String(): Unit = {
    val read = input(TextNewLine).slurpString(UTF8)

    val expected = TEXT_VALUE

    assertEquals(expected, read)
  }

  @Test(timeout = 3000) //@Ignore
  def read_lines_Auto_stackoverflow_bug: Unit = {
    import JavaConverters._

    val data = "l1\nl2\nlastline not terminator"
    val lines = data.asReadChars.lines()
    assertEquals(data.split("\\n").toList, lines.toList)
  }

  @Test//(timeout = 3000) //@Ignore
  def read_all_lines_Auto: Unit = {
    testLines("NewLine", TextCustomData("\n", "\n"), Auto, false)
    testLines("NewLine", TextCustomData("\n", "aa\n"), Auto, false)
    testLines("NewLine", TextCustomData(RNPair.sep, "aa" + RNPair.sep), Auto, false)
    testLines("NewLine", TextNewLine, Auto, false)
    testLines("Pair", TextPair, Auto, false)
    testLines("CarriageReturn", TextCarriageReturn, Auto, false)

    testLines("include NewLine", TextNewLine, Auto, true)
    testLines("include Pair", TextPair, Auto, true)
    testLines("include CarriageReturn", TextCarriageReturn, Auto, true)
  }

  @Test //(timeout = 3000) //@Ignore
  def read_all_lines(): Unit = {
    testLines("NewLine", TextNewLine, NewLine, false)
    testLines("Pair", TextPair, RNPair, false)
    testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, false)
    testLines("Custom", TextCustom("x"), Custom("x"), false)
  }

  @Test(timeout = 3000) //@Ignore
  def read_all_lines_includeTerminator(): Unit = {
    testLines("Auto", TextNewLine, Auto, true)
    testLines("NewLine", TextNewLine, NewLine, true)
    testLines("Pair", TextPair, RNPair, true)
    testLines("CarriageReturn", TextCarriageReturn, CarriageReturn, true)
    testLines("Custom", TextCustom("x"), Custom("x"), true)
  }

  def testLines(msg: String, t: Text, terminator: Terminator, include: Boolean) {
    val read = input(t).lines(terminator, include)(UTF8).toList
    val expected = {
      val (sep, data) = t match {
        case TextCustomData(sep, data) => (sep, data)
        case _ => ("\n", TEXT_VALUE)
      }
      val lines = data.split(sep).toList

      val withLastEl =
        if (data.matches("(?ms).*\\s+" + sep) || data.matches("(?ms)\\s*" + sep)) lines :+ ""
        else lines

      if (include) withLastEl.map {
        _ + t.sep
      }
      else withLastEl
    }
    def display(c: List[String]) = c.map(_.replace("\n", "\\n").replace("\r", "\\r"))
    assert(expected == read, "expected: " + display(expected) + " but was " + display(read));
  }

  @Test(timeout = 3000) //@Ignore
  def read_some_lines(): Unit = {
    val read = input(TextNewLine).lines()(UTF8).drop(2).take(2).toList
    val expected = TEXT_VALUE.split("\n").toList.drop(2).take(2)
    assertEquals(expected, read)
  }

  @Test(timeout = 3000) //@Ignore
  def lines_toString_does_not_resolve_list(): Unit = {
    val read = input(TextNewLine).lines()(UTF8).toString
    val textExpected = TEXT_VALUE.split("\n").toString
    assertFalse(read contains textExpected)
  }

  @Test //(timeout = 3000) //@Ignore
  def copyDataTo(): Unit = {
    val outStream = new ByteArrayOutputStream()
    input(TextNewLine).copyDataTo(outStream.asOutput)

    val expected = TEXT_VALUE

    assertEquals(expected, new String(outStream.toByteArray, "UTF-8"))
  }

  @Test //(timeout = 3000) //@Ignore
  def copyDataToWithChannel(): Unit = {
    val outStream = new ByteArrayOutputStream()
    val outChan = Channels.newChannel(outStream)
    input(TextNewLine).copyDataTo(outChan.asOutput)

    val expected = TEXT_VALUE

    assertEquals(expected, new String(outStream.toByteArray, "UTF-8"))
  }
  def copyDataToFile(): Unit = {
    val file = File.createTempFile(getClass.getSimpleName(), "tmp")
    val outChan = Channels.newChannel(new FileOutputStream(file))
    input(TextNewLine).copyDataTo(outChan.asOutput)

    val expected = TEXT_VALUE

    assertEquals(expected, new String(Resource.fromFile(file).byteArray, "UTF-8"))
  }

  @Test(timeout = 3000) //@Ignore
  def byteCountForLargeInput(): Unit = {
    val text = (1 to (8 * 1024) flatMap { _ => TEXT_VALUE }).mkString
    val in = input(TextCustomData("\n", text))

    assertEquals(text.getBytes("UTF-8").length, in.bytes.size)
  }

  @Test
  def scalaIoException_On_Read_Error_by_default {
    intercept[ScalaIOException] {
      input(ErrorOnRead).bytes.head
    }
  }

  @Test
  def scalaIoException_On_Close_Error_by_default {
    intercept[ScalaIOException] {
      input(ErrorOnClose).bytes.head
    }
  }
  @Test
  def assertNoExceptionsRaisedOnInputMethodCalls() {
    assumeNotWindows
    val errorOnReadInput = input(ErrorOnRead)
    if (errorOnReadInput.isInstanceOf[Resource[_]]) {
      class MyException extends Exception
      val context = new ResourceContext {
        override def openErrorHandler[A, U](f: A => U, openException: Throwable): Option[U] =
          throw new MyException
        override def errorHandler[A, U](f: A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U =
          throw new MyException
      }
      val input = errorOnReadInput.asInstanceOf[Resource[_]].
        updateContext(context).
        asInstanceOf[Input]

      intercept[MyException](input.blocks(Some(2)).foreach(_ => ()))
      intercept[MyException](input.blocks().foreach(_ => ()))
      intercept[MyException](input.blocks(Some(1000000)).foreach(_ => ()))
      intercept[MyException](input.blocks(Some(2)).force)
      intercept[MyException](input.byteArray)
      intercept[MyException](input.slurpString())
      intercept[MyException](input.slurpString(Codec.ISO8859))
      intercept[MyException](input.bytes.foreach(_ => ()))
      intercept[MyException](input.bytes.force)
      intercept[MyException](input.bytesAsInts.force)
      intercept[MyException](input.bytesAsInts.foreach(_ => ()))
      intercept[MyException](input.chars().foreach(_ => ()))
      intercept[MyException](input.chars().force)
      intercept[MyException](input.chars(Codec.ISO8859).foreach(_ => ()))
      intercept[MyException](input.chars(Codec.ISO8859).force)
      intercept[MyException](input.lines(Line.Terminators.Auto, false)(Codec.UTF8).force)
      intercept[MyException](input.lines(Line.Terminators.Auto, false)(Codec.UTF8).foreach(_ => ()))
    }
  }
  @Test
  def customErrorHandler_On_Read_Error {
    assumeNotWindows
    val testContext = new ErrorHandlingTestContext()

    val errorOnReadInput = input(ErrorOnRead)
    if (errorOnReadInput.isInstanceOf[Resource[_]]) {
      val customHandlerInput = errorOnReadInput.asInstanceOf[Resource[_]].
        updateContext(testContext.customContext).
        asInstanceOf[Input]
      customHandlerInput.bytes.headOption
      assertEquals(1, testContext.accessExceptions + testContext.openExceptions) // Sometimes access error occurs during opening resource like with files
      assertEquals(0, testContext.closeExceptions)
    }
  }
  @Test
  def customErrorHandler_On_Close_Error {
    val testContext = new ErrorHandlingTestContext()

    val errorOnCloseInput = input(ErrorOnClose)
    if (errorOnCloseInput.isInstanceOf[Resource[_]]) {
      val customHandlerInput = errorOnCloseInput.asInstanceOf[Resource[_]].
        updateContext(testContext.customContext).
        asInstanceOf[Input]
      customHandlerInput.bytes.headOption
      assertEquals(0, testContext.accessExceptions)
      assertTrue(testContext.closeExceptions >= 1)
    }
  }
  @Test
  def customErrorHandler_On_AcquireAndGet {
    val testContext = new ErrorHandlingTestContext()

    val goodInput = input(Image)

    if (goodInput.isInstanceOf[Resource[_]]) {
      val customHandlerInput = goodInput.asInstanceOf[Resource[_]].
        updateContext(testContext.customContext)
      customHandlerInput.acquireAndGet(_ => assert(false))
      assertEquals(1, testContext.accessExceptions)
      assertEquals(0, testContext.closeExceptions)
    }
  }

  @Test
  def custom_ErrorHandler_can_return_default_string {
    assumeNotWindows

    val default = "Default".getBytes("UTF-8")
    val context = new ResourceContext {
      override def openErrorHandler[A, U](f: A => U, openException: Throwable): Option[U] = {
        Some(default.asInstanceOf[U])
      }
      override def errorHandler[A, U](f: A => U, accessResult: Either[Throwable, U], closingExceptions: List[Throwable]): U = {
        default.asInstanceOf[U]
      }
    }
    val resource = input(ErrorOnRead)
    if (resource.isInstanceOf[Resource[_]]) {
      val customHandlerInput = resource.asInstanceOf[Resource[_]].
        updateContext(context).
        asInstanceOf[Input]

      assertEquals("Default", customHandlerInput.slurpString())
    }
  }

  @Test
  def input_closed_after_each_action {

    import JavaConverters._
    var opens = 0
    var closes = 0
    val in = input(TextNewLine, () => opens += 1, () => closes += 1)

    opens = 0
    closes = 0

    def assertCorrectOpens[U](function: => U) = {
      canExecuteOpenFunction match {
        case true => assertEquals(opens, closes)
        case false => assertTrue(closes > 1)
      }
      opens = 0
      closes = 0
    }

    assertCorrectOpens(in.blocks(None).force)
    assertCorrectOpens(in.blocks(None).headOption)
    assertCorrectOpens(in.byteArray)
    assertCorrectOpens(in.bytes.headOption)
    assertCorrectOpens(in.bytes.force)
    assertCorrectOpens(in.bytesAsInts.headOption)
    assertCorrectOpens(in.bytesAsInts.force)
    assertCorrectOpens(in.chars().force)
    assertCorrectOpens(in.chars().headOption)
    assertCorrectOpens(in.copyDataTo(Resource.fromOutputStream(new ByteArrayOutputStream())))
    assertCorrectOpens(in.lines()(Codec.UTF8).force)
    assertCorrectOpens(in.lines()(Codec.UTF8).headOption)
    assertCorrectOpens(in.slurpString())
  }
}
