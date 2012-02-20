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

  @Test//(timeout=3000L)
  def open_multiple_writes {
    val (input, output) = open()
    val line1 = "line1"
    val line2 = "line2"
    for{
      out <- output.outputProcessor
    } {
      out.write(line1)
      out.write(line2)
    }
    assertEquals(line1+line2,input.slurpString)
  }

  @Test //@Ignore
  def openOutput: Unit = {
    var closes = 0;
    val (in,out) = open(CloseAction((c:Any) => closes += 1))
    out match {
      case out:OutputResource[_] =>

        assertEquals(0,closes)
        out.write("whoop!")
        assertEquals(1,closes)

        for (opened <- out.outputProcessor) {
          opened.write("hello")
          opened.write(" ")
          opened.write("world")
        }
        assertEquals(2,closes)
        assertEquals("whoop!hello world",in.slurpString)
      case _ => ()
    }
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

  @Test
  def customErrorHandler_On_Write_Error{
    val testContext = new ErrorHandlingTestContext() 

    val errorOnReadOutput = errorOnWriteOut

    if(errorOnReadOutput.isInstanceOf[Resource[_]]) {
      val customHandlerOutput = errorOnReadOutput.asInstanceOf[Resource[_]].
                                  updateContext(testContext.customContext).
                                  asInstanceOf[Output]
      customHandlerOutput.write("hi")
      assertEquals(if(writeErrorRaisedOnClose) 0 else 1, testContext.accessExceptions + testContext.openExceptions)
      assertEquals(if(writeErrorRaisedOnClose) 1 else 0, testContext.closeExceptions)
      assertNoExceptionsRaisedOnOutputMethodCalls(customHandlerOutput)
    }
  }
  @Test
  def customErrorHandler_On_Write_Close_Error{
    val testContext = new ErrorHandlingTestContext() 

    val errorOnCloseOutput = open(CloseAction(_ => throw new IOException("CloseError")))._2
    if (errorOnCloseOutput.isInstanceOf[Resource[_]]) {
      val customHandlerOutput = errorOnCloseOutput.asInstanceOf[Resource[_]].
                                  updateContext(testContext.customContext).
                                  asInstanceOf[Output]
      customHandlerOutput.write("hi")
      assertEquals(0, testContext.accessExceptions)
      assertEquals(1, testContext.closeExceptions)
      assertNoExceptionsRaisedOnOutputMethodCalls(customHandlerOutput)
    }
  }
  @Test
  def customErrorHandler_On_Write_AcquireAndGet {
    val testContext = new ErrorHandlingTestContext() 
    val (_,goodOutput) = open()
    
    if (goodOutput.isInstanceOf[Resource[_]]) {
      val customHandlerInput = goodOutput.asInstanceOf[Resource[_]].
        updateContext(testContext.customContext)
      customHandlerInput.acquireAndGet(_ => assert(false))
      assertEquals(1, testContext.accessExceptions)
      assertEquals(0, testContext.closeExceptions)
    }
  }

}
