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
import org.junit.{
Test, Ignore
}

import Constants.TEXT_VALUE
import org.junit.Assert._

abstract class AbstractSeekableTests extends scalax.test.sugar.AssertionSugar {
  implicit val codec = Codec.UTF8

  /**
   * Seekable containing TEXT_VALUE, otherwise
   */
  def open(data: String = TEXT_VALUE): Seekable


  val patchParams =
   /* ("replaced is MaxValue", 2, "ア", Some(Int.MaxValue)) ::
      ("too large max", 2, "ア", Some(8)) ::
      ("basic", 2, "ア", None) ::
      ("insert", 2, "ア", Some(-1)) ::
      ("start at 0", 0, "ア", None) ::
      ("to large position", 199, "ア", None) ::
      ("very large patch", 2, (1 to 100 mkString ""), None) ::
      ("0 length", 2, "ア", Some(0)) ::
      ("partial overwite", 2, "its a long one!", Some(3)) ::*/
      ("overwite All at a far position", 10, "\u2248\u2248", None) ::
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
    val seekable = open("abc")

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
    val seekable = open("abc")

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
      val seekable = open("abc")
      try {
        f(seekable)
      } catch {
        case e =>
          val error = new Error(msg + " failed due to: " + e)
          error.setStackTrace(e.getStackTrace)
          throw error
      }
      val expected = "abc".getBytes(UTF8.name).toList.patch(pos, bytes.toSeq, -1)
      val actual = seekable.bytes.toList
      assertEquals(msg, expected, actual)
    }
    def run[T](msg: String, pos: Int, data: String) {
      val bytes = data.getBytes(UTF8.name)
      test("Array: "+msg, _.insert(pos,bytes), pos, bytes)
      test("Iterator: " + msg, _.insert(pos, bytes.toIterator), pos, bytes)
      test("List: "+msg,_.insert(pos,bytes.toList), pos, bytes)
      test("String: "+msg,_.insert(pos,data), pos, bytes)
      test("noDefinateSize: "+msg,_.insert(pos,data), pos, bytes)
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
    seekable.truncateString(2)
    assertEquals(expected, seekable.slurpString)
  }

  @Test //@Ignore
  def truncate: Unit = {
    val seekable = open()
    val expected = TEXT_VALUE take 2
    seekable.truncate((UTF8 encode expected)size)
    assertEquals(expected, seekable.slurpString)

    seekable.truncate(0)
    assertEquals("", seekable.slurpString)
    seekable.append("more")
    assertEquals("more", seekable.slurpString)

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

  @Test //@Ignore
  def openSeekable: Unit = {
    var closes = 0;
    val seekable = open() match {
      case raw:SeekableResource[_] =>
        val seekable = raw.appendCloseAction(_ => closes += 1)
        assertEquals(0,closes)
        seekable.write("whoop!")
        assertEquals(1, closes)
        seekable.open(opened => {
          opened.truncate(0)
          opened.write("hello-")
          opened.write("world")

          opened.position = 5
          opened.write(' ')
        })
        assertEquals(2, closes)
        assertEquals("hello world", seekable.slurpString)
      case _ => ()
    }
  }

  @Test //@Ignore
  def openSeekableReadAndWrite: Unit = {
    var closes = 0;
    val seekable = open() match {
      case raw:SeekableResource[_] =>
        val seekable = raw.appendCloseAction(_ => closes += 1)

        val data = "it is a wonderful world"
        val expectedEnd = "it is a fantastic place to be"

        seekable.open(opened => {
          opened.truncate(0)
          opened.write(data)

          assertEquals(data, opened.slurpString)

          val pos = opened.chars.indexOfSlice("wonderful")
          opened.patch(pos,"fantastic", OverwriteSome("wonderful".size))

          assertEquals("it is a fantastic world", opened.slurpString)

          val worldPos = opened.bytes.indexOfSlice("world".getBytes(Codec.UTF8.charSet))
          opened.position = worldPos
          opened.write("place to be")
          assertEquals(expectedEnd, opened.slurpString)
        })
        assertEquals(1, closes)
        assertEquals(expectedEnd, seekable.slurpString)
      case _ => ()
    }
  }


  @Test //@Ignore
  def interleavingReadWrite: Unit = {
    val testData = "12345"
    val appendedData = "09876"
    val testDataAsBytes: Array[Byte] = testData.getBytes(UTF8.charSet)
    val appendedDataAsBytes: Array[Byte] = appendedData.getBytes(UTF8.charSet)

    def perform(seekable:Seekable,msg:String) {
      val bytes = seekable.bytes
      val firstPart = bytes.take(5)

      assertEquals(msg+": checking firstpart before write", testDataAsBytes.mkString, firstPart.mkString)
      seekable.append(appendedData)
      val secondPart = bytes.drop(5)
      assertEquals(msg+": verifying that data is correctly appended", (testData + appendedData).getBytes(UTF8.charSet).mkString, seekable.bytes.mkString)
      assertEquals(msg+": checking that bytes correctly gets the appended data but not first part", appendedDataAsBytes.mkString, secondPart.mkString)
      assertEquals(msg+": checking that the first part traversable still works", testDataAsBytes.mkString, firstPart.mkString)

      val zipped = firstPart.zip(secondPart)

      val expectedZip: Array[(Byte, Byte)] = testDataAsBytes.zip(appendedData.getBytes(UTF8.charSet))

      assertEquals(msg+": does not correctly handle the same data zipped together in same openned resource", expectedZip.mkString, zipped.mkString)
    }

    val seekable = open()
    seekable.truncate(0)
    seekable.write(testData)
    perform(seekable, "basic seekable")

    seekable.truncate(0)

    seekable.write(testData)

    seekable.open(perform(_, "opened seekable"))
  }

}
