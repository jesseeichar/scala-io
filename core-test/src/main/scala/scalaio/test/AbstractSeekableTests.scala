/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import Codec.UTF8
import org.junit.Assert._
import org.junit.{
Test, Ignore
}

import Constants.TEXT_VALUE

abstract class AbstractSeekableTests extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8

  /**
   * Seekable containing TEXT_VALUE
   */
  def open(data: Option[String] = None): Seekable


  val patchParams =
    ("replaced is MaxValue", 2, "ア", Some(Int.MaxValue)) ::
      ("too large max", 2, "ア", Some(8)) ::
      ("basic", 2, "ア", None) ::
      ("insert", 2, "ア", Some(-1)) ::
      ("start at 0", 0, "ア", None) ::
      ("to large position", 199, "ア", None) ::
      ("very large patch", 2, (1 to 100 mkString ""), None) ::
      ("0 length", 2, "ア", Some(0)) ::
      ("partial overwite", 2, "its a long one!", Some(3)) ::
      Nil

  @Test //@Ignore
  def patchString(): Unit = {

    val testFunction = Function tupled testPatchString _
    patchParams foreach testFunction

    intercept[IllegalArgumentException] {
      open().patch(-1, "@", OverwriteSome(3))
    }
    // test UTF16?
  }

  @Test //@Ignore
  def patchStringASCII(): Unit = {
    val seekable = open(Some("abc"))

    seekable.patch(1, "x", OverwriteSome(1))(Codec.ISO8859)

    assertEquals("axc", seekable.slurpString)
  }

  private def testPatchString(msg: String, from: Int, data: String, length: Option[Int]) = {
    val seekable = open()

    val p = (TEXT_VALUE.replaceAll("\n", "\\n"), from.min(Int.MaxValue).toInt, data, data.size)
    val expected = length match {
      case Some(length) => TEXT_VALUE.toList.patch(from.min(Int.MaxValue).toInt, data, length).mkString
      case None => TEXT_VALUE.toList.patch(from.min(Int.MaxValue).toInt, data, data.size).mkString
    }

    length match {
      case Some(length) => seekable.patch(from, data, OverwriteSome(length))
      case None => seekable.patch(from, data, OverwriteAll)
    }
    assertEquals(msg, expected, seekable.slurpString)
  }

  @Test //@Ignore
  def patchWithIterator(): Unit = {
    val seekable = open(Some("abc"))

    seekable.patch(1, "x".getBytes(UTF8.name).toIterator, OverwriteAll)

    assertEquals("axc", seekable.slurpString)
  }

  @Test //@Ignore
  def insert(): Unit = {
    val inputData =
      ("pos less than 0", -1, "x") ::
        ("pos == 0", 0, "x") ::
        ("pos 1", 1, "x") ::
        ("pos to large", 10, "x") ::
        Nil


    def test[T](msg: String, f: Seekable => T, pos: Int, bytes: Array[Byte]) = {
      val seekable = open(Some("abc"))
      try {
        f(seekable)
      } catch {
        case e =>
          val error = new Error(msg + " failed due to: " + e)
          error.setStackTrace(e.getStackTrace)
          throw error
      }
      assertEquals(msg, "abc".getBytes(UTF8.name).toList.patch(pos, bytes.toSeq, -1), seekable.bytes.toList)
    }
    def run[T](msg: String, pos: Int, data: String) {
      val bytes = data.getBytes(UTF8.name)
      //test("Array: "+msg, _.insert(pos,bytes), pos, bytes)
      test("Iterator: " + msg, _.insert(pos, bytes.toIterator), pos, bytes)
      //test("List: "+msg,_.insert(pos,bytes.toList), pos, bytes)
      //test("String: "+msg,_.insert(pos,data), pos, bytes)
    }

    val func = Function tupled (run _)
    inputData foreach func
  }

  @Test //@Ignore
  def patch(): Unit = {
    val testFunction = Function tupled testPatch _
    patchParams foreach testFunction

    intercept[IllegalArgumentException] {
      open().patch(-1, "@".getBytes(UTF8.name), OverwriteSome(3))
    }
  }


  private def testPatch(msg: String, fromInChars: Int, dataString: String, lengthInChars: Option[Int]) = {

    val MAX_VALUE = Int.MaxValue

    val from = TEXT_VALUE.take(fromInChars).getBytes(UTF8.name).size
    val length: Option[Long] = lengthInChars match {
      case Some(MAX_VALUE) => Some(Long.MaxValue)
      case Some(-1) => Some(-1L)
      case Some(lengthInChars) => Some(TEXT_VALUE.slice(fromInChars, fromInChars + lengthInChars).getBytes(UTF8.name).size toLong)
      case None => None
    }

    val data = dataString.getBytes(UTF8.name)

    def test[T](datatype: String, f: (Seekable, Long, Array[Byte], Overwrite) => T) = {
      val seekable = open()
      assertEquals(TEXT_VALUE, seekable.slurpString)

      val expected = lengthInChars match {
        case None => TEXT_VALUE.getBytes(UTF8.name) patch (from, data, data.size)
        case Some(lengthInChars) if (lengthInChars == MAX_VALUE) => TEXT_VALUE.getBytes(UTF8.name) patch (from, data, MAX_VALUE)
        case Some(lengthInChars) => TEXT_VALUE.getBytes(UTF8.name) patch (from, data, length.get.toInt)
      }

      length match {
        case Some(length) => f(seekable, from, data, OverwriteSome(length))
        case None => f(seekable, from, data, OverwriteAll)
      }


      assertEquals(datatype + " - patch: '" + msg + "'", expected mkString ",", seekable.byteArray mkString ",")
    }

    test("array", (s, a, b, c) => s.patch(a, b, c))
    test("list", (s, a, b, c) => s.patch(a, b.toList, c))
    test("stream", (s, a, b, c) => s.patch(a, b.toStream, c))
    test("iterator", (s, a, b, c) => s.patch(a, b.toIterator, c))

  }

  @Test //@Ignore
  def appendString: Unit = {
    val data = "�?�"
    val seekable = open()
    val expected = TEXT_VALUE + data
    seekable.append(data)
    assertEquals(expected, seekable.slurpString)
  }

  @Test //@Ignore
  def appendStrings: Unit = {
    val data = "�?�" :: "%" :: "~µ" :: Nil

    def test(sep: String) = {
      val seekable = open()
      val expected = TEXT_VALUE + (data mkString sep)
      seekable.appendStrings(data, sep)
      assertEquals(expected, seekable.slurpString)
    }

    val seekable = open()
    val expected = TEXT_VALUE + (data mkString "")
    seekable.appendStrings(data)
    assertEquals(expected, seekable.slurpString)

    test("123")
    test(",")
  }

  @Test //@Ignore
  def chopString: Unit = {
    val seekable = open()
    val expected = TEXT_VALUE take 2
    seekable.chopString(2)
    assertEquals(expected, seekable.slurpString)
  }

  @Test //@Ignore
  def chop: Unit = {
    val seekable = open()
    val expected = TEXT_VALUE take 2
    seekable.chop(UTF8 encode expected size)
    assertEquals(expected, seekable.slurpString)
  }

  @Test //@Ignore
  def append: Unit = {
    def test(_type: Int) = {
      val data: String = "�?�"
      val seekable = open()
      val expected = TEXT_VALUE + data
      val bytes = data.getBytes(UTF8.name)
      _type match {
        case 0 => seekable.append(bytes)
        case 1 => seekable.append(bytes.toList)
        case 2 => seekable.append(bytes.toIterator)
      }
      assertEquals("append " + _type, expected, seekable.slurpString)
    }
    0 to 2 foreach test
  }
}
