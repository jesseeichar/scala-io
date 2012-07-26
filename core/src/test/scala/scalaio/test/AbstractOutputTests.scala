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
import java.io.IOException
import java.io.OutputStream
import scalax.io.processing.Processor

trait AbstractOutputTests[InResource, OutResource] extends scalax.test.sugar.AssertionSugar {
  private final val DEFAULT_DATA = "default data"
  implicit val codec = Codec.UTF8

  def open(closeAction:CloseAction[OutResource] = CloseAction.Noop): (Input, Output)
  def errorOnWriteOut:Output
  final def errorStream = new OutputStream(){
    override def write(b:Array[Byte], off:Int, len:Int) = throw new IOException("error")
    override def write(b:Int) = throw new IOException("error")
  }
  @Test //@Ignore
  def write_bytes(): Unit = {
    val (input, output) = open()
    val bytes = DEFAULT_DATA.getBytes

    output write bytes

    assertArrayEquals(bytes, input.byteArray)
  }

  @Test //@Ignore
  def write_string(): Unit = {

    val (input, output) = open()

    output write DEFAULT_DATA

    assertEquals(DEFAULT_DATA, input.slurpString)
  }

  @Test //@Ignore
  def write_charseq(): Unit = {

    val (input, output) = open()
    val charSeq = new StringBuilder(DEFAULT_DATA)

    output writeChars charSeq

    assertEquals(DEFAULT_DATA, input.slurpString)
  }

  @Test //@Ignore
  def write_traversable_char(): Unit = {

    val (input, output) = open()

    output writeChars DEFAULT_DATA.toList

    assertEquals(DEFAULT_DATA, input.slurpString)
  }


  @Test //@Ignore
  def write_many_strings(): Unit = {

    val (input, output) = open()

    output writeStrings (DEFAULT_DATA :: DEFAULT_DATA :: DEFAULT_DATA :: Nil)
    assertEquals(DEFAULT_DATA + DEFAULT_DATA + DEFAULT_DATA, input.slurpString)

    val (input2, output2) = open()

    output2 writeStrings (DEFAULT_DATA :: DEFAULT_DATA :: DEFAULT_DATA :: Nil, "-")
    assertEquals(DEFAULT_DATA + "-" + DEFAULT_DATA + "-" + DEFAULT_DATA, input2.slurpString)
  }

  @Test //@Ignore
  def write_blocks(): Unit = {
    import JavaConverters._
    val (input, output) = open()
    val testData = (1 to 100).flatMap(_.toString.getBytes(Codec.UTF8.charSet)).asInput

    testData.blocks().foreach(block => output.write(block))

    assertEquals(testData.slurpString, input.slurpString)
  }

  @Test//(timeout=3000L)
  def open_multiple_writes {
    val (input, output) = open()
    val line1 = "line1"
    val line2 = "line2"
    for{
      outp <- output.outputProcessor
      out = outp.asOutput
    } {
      out.write(line1)
      out.write(line2)
    }
    assertEquals(line1+line2,input.slurpString)
  }
  @Test
  def processor_style_writing {
    import scalax.io.JavaConverters._
        val (in, out) = open()
        val process = for {
      inp <- (1 to 4).asInput.bytes.processor
      in2 <- "hi".getBytes().asInput.bytes.processor
      outp <- out.outputProcessor
      _ <- inp.repeatUntilEmpty()
      next <- inp.next
      _ <- outp.write(next.toString)
      _ <- in2.repeatUntilEmpty()
      letter <- in2.next
      _ <- outp.write(letter.toChar.toString)
      out = outp.asOutput
    } yield {
      out.write("-")
    }
    assertTrue(in.slurpString.isEmpty)
    process.execute()
    assertEquals("1h-i-234", in.slurpString)
  }

  /**
   *
   * @return true
   *      if the each time the output is opened the underlying data is cleared.
   *      this is how a file will behave if not opened as append.
   */
  def truncatesUnderlyingSinkEachOpen = false
  @Test //@Ignore
  def write_and_processor_write_combo: Unit = {
    var closes = 0;
    val (in,out) = open(CloseAction((c:Any) => closes += 1))
    out match {
      case out:OutputResource[_] =>

        assertEquals(0,closes)
        out.write("whoop!")
        assertEquals(1,closes)

        for (opened <- out.outputProcessor) {
          opened.write("hello").execute
          opened.write(" ").execute
          opened.write("world").execute
        }
        assertEquals(2,closes)
        val expected =
          if(truncatesUnderlyingSinkEachOpen) "hello world"
          else "whoop!hello world"
        assertEquals(expected,in.slurpString)
      case _ => ()
    }
  }

  @Test
  def execute_executes_a_traversable_processor {
    import scalax.io.JavaConverters._
    val (in, out) = open()
    val processor2 = for {
      inp <- (1 to 10).asInput.bytes.processor
      outp <- out.outputProcessor
      _ <- outp.write("start")
      _ <- inp.repeatUntilEmpty()
      next <- inp.next
      _ <- outp.write(next.toString)
    } yield {}
    assertEquals("", in.slurpString)
    processor2.execute
    assertEquals("start12345678910", in.slurpString)
  }

  @Test
  def scalaIoException_On_Write_Error_by_default{
    intercept[ScalaIOException] {
      errorOnWriteOut.write("hi")
    }
  }

  @Test
  def scalaIoException_On_Write_Close_Error_by_default{
    intercept[ScalaIOException] {
       val output = open(CloseAction(_ =>
         throw new IOException("CloseError")
         ))._2
     output.write("hi")
    }
  }
  def writeErrorRaisedOnClose = false
  def assertNoExceptionsRaisedOnOutputMethodCalls(out:Output) {
    import scalax.io.JavaConverters._
    List(1,2,3).asInput.copyDataTo(out)
    out.write("hi")
    out.write(List(1,2,3))
    out.write(List(1.toByte))
    out.write(3)
    out.writeChars("hi")
    out.writeIntsAsBytes(1,2,3,4)
    out.writeStrings(List("1","2"))
  }

}
