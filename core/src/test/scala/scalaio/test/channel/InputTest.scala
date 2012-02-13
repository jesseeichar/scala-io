/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.channel

import scalaio.test._
import java.io.ByteArrayInputStream
import scalax.io.{Codec, Resource}
import Resource._
import org.junit.Test
import org.junit.Assert._
import java.lang.String
import java.nio.channels.Channels
import scalaio.test.LongTraversableTest
import scalax.io.LongTraversable
import scalax.io.Input

class InputTest extends AbstractInputTests with DataIndependentLongTraversableTest[Byte] {
  implicit val codec = Codec.UTF8
  def independentTraversable = {
    val data = (1 to 100).map(_ => '1').mkString
    val bytes = data.getBytes(Codec.UTF8.charSet)
    val size = bytes.size
    val resource = input(TextCustomData("",data))
    resource.bytes
  }
  val sample = Array[Byte](1,3,4)
  val independentExpectedData:Seq[Byte] = (1 to 100).map(_ => 49.toByte)
  def times(t1:Byte, t2:Byte):Byte = (t1 * t2).toByte
  def lessThan(t:Byte,i:Int):Boolean = t < i
  def scanSeed = 2.toByte
  
  protected def textResource(sep: String):Input =
    fromReadableByteChannel(Channels.newChannel(new java.io.ByteArrayInputStream(text(sep))))
  protected def text(sep: String) = {
    val finalText: String = Constants.TEXT_VALUE.replaceAll("""\n""", sep)
    finalText.getBytes(Codec.UTF8.charSet)
  }
  protected def customDataResource(data:String):Input = {
    fromReadableByteChannel(
      Channels.newChannel(new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet)))
    )
  }
  protected def imageResource:Input= {
    fromReadableByteChannel(Channels.newChannel(Constants.IMAGE.openStream()))
  }
  protected def input(t: Type) = t match {
    case t@TextNewLine => textResource(t.sep)
    case t@TextPair => textResource(t.sep)
    case t@TextCarriageReturn => textResource(t.sep)
    case TextCustom(sep) => textResource(sep)
    case TextCustomData(sep, data) => customDataResource(data)
    case Image => imageResource
    case ErrorOnRead => fromReadableByteChannel(Channels.newChannel(ErrorOnRead.errorInputStream))
    case ErrorOnClose => fromReadableByteChannel(Channels.newChannel(ErrorOnClose.errorInputStream))
  }

  override protected def sizeIsDefined = false

  @Test
  def issue_8_lines_in_Input_not_lazy {
    import scalax.io.Line.Terminators._

    val file = largeResource(Key.TEXT)
    
    val start = System.currentTimeMillis
    val fromFile = Resource.fromFile(file).lines(NewLine)
    val fromString = Resource.fromFile(file.getAbsolutePath).lines(NewLine)
    fromString.toString
    fromFile.toString
    val end = System.currentTimeMillis
    assertTrue(end-start < 500)
  }

}

