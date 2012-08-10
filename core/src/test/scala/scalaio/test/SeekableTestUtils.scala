package scalaio.test

import language.reflectiveCalls
import scalax.io.SeekableResource
import scalax.io.Seekable
import scalax.io.Input
import scalax.io.Output
import scalax.io.managed.InputStreamResource
import scalax.io.CloseAction
import scalax.io.Resource
import scalaio.test.AbstractInputTests._
import java.io.InputStream
import scalax.io.Codec

trait SeekableTestUtils[ResourceType] {
  protected def forceErrorOnAccess():Unit
  protected def openResource(openFunction: () => Unit, closeAction: CloseAction[ResourceType]): Seekable
  def openSeekable(data: String, openFunction: () => Unit, closeFunction: () => Unit) = {
    val r = openResource(openFunction, CloseAction[ResourceType](_ => closeFunction()))
    r truncate 0
    r write data
    r
  }
  def open(closeAction: CloseAction[ResourceType] = CloseAction.Noop): (Input, Output) = {
    val r = openResource(() => (), closeAction)
    (r, r)
  }
  def errorOnWriteOut: Seekable = {
    val resource = openResource(() => (), CloseAction.Noop)
    forceErrorOnAccess()
    resource
  }

  def input(t: Type, openFunction: () => Unit, closeFunction: () => Unit) = (t: @unchecked) match {
    case t @ TextNewLine => text(t.sep,openFunction,closeFunction)
    case t @ TextPair => text(t.sep,openFunction,closeFunction)
    case t @ TextCarriageReturn => text(t.sep,openFunction,closeFunction)
    case TextCustom(sep) => text(sep,openFunction,closeFunction)
    case TextCustomData(sep, data) =>
      val s = openResource(openFunction,CloseAction.Noop)
      s truncate 0
      s.write(data)(Codec.UTF8)
      if(s.isInstanceOf[Resource[_]]) {
          s.asInstanceOf[Resource[_]].addCloseAction(CloseAction(_ => closeFunction()))
      }
      s
    case Image => image(openFunction,closeFunction)
    case ErrorOnRead => errorOnWriteOut
    case ErrorOnClose => openResource(() => (), CloseAction(_ => throw new AssertionError("boom")))

  }
  def image(openFunction: () => Unit, closeFunction:() => Unit) =
    copyResource(Resource.fromInputStream(Constants.IMAGE.openStream()), openFunction, closeFunction)

  def text(sep: String, openFunction: () => Unit, closeFunction:() => Unit) = {
    val resource = Resource.fromInputStream {
      val bytes = Constants.TEXT_VALUE.replaceAll("""\n""", sep).getBytes(Codec.UTF8.name)
      new java.io.ByteArrayInputStream(bytes)
    }
    copyResource(resource, openFunction, closeFunction)
  }
  def copyResource(source : InputStreamResource[InputStream],openFunction: () => Unit,closeFunction:() => Unit) : Seekable = {
      val dest = openResource(openFunction, CloseAction(_ => closeFunction))
      dest truncate 0
      dest write (source.bytes)
      dest
  }

}
