package scalaio.test
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
import scalax.io.SeekableByteChannel

trait SeekableTestUtils[ResourceType] {
  protected def deleteResource():Unit
  protected def openResource(closeAction: CloseAction[ResourceType]): Seekable
  def openSeekable(data: String, closeFunction: () => Unit) = {
    val r = openResource(CloseAction[ResourceType](_ => closeFunction()))
    r write data
    r
  }
  def open(closeAction: CloseAction[ResourceType] = CloseAction.Noop): (Input, Output) = {
    val r = openResource(closeAction)
    (r, r)
  }
  def errorOnWriteOut: Seekable = {
    val r = openResource(CloseAction.Noop)
    deleteResource()
    r
  }

  def input(t: Type, closeFunction: () => Unit) = t match {
    case t @ TextNewLine => text(t.sep,closeFunction)
    case t @ TextPair => text(t.sep,closeFunction)
    case t @ TextCarriageReturn => text(t.sep,closeFunction)
    case TextCustom(sep) => text(sep,closeFunction)
    case TextCustomData(sep, data) =>
      val s = openResource(CloseAction.Noop)
      s.write(data)(Codec.UTF8)
      if(s.isInstanceOf[Resource[_]]) {
          s.asInstanceOf[Resource[_]].addCloseAction(CloseAction(_ => closeFunction()))
      }
      s
    case Image => image(closeFunction)
    case ErrorOnRead => errorOnWriteOut
    case ErrorOnClose => openResource(CloseAction(_ => throw new AssertionError("boom")))

  }
  def image(closeFunction:() => Unit) = copyResource(Resource.fromInputStream(Constants.IMAGE.openStream()), closeFunction)

  def text(sep: String, closeFunction:() => Unit) = {
    val resource = Resource.fromInputStream {
      val bytes = Constants.TEXT_VALUE.replaceAll("""\n""", sep).getBytes(Codec.UTF8.name)
      new java.io.ByteArrayInputStream(bytes)
    }
    copyResource(resource, closeFunction)
  }
  def copyResource(source : InputStreamResource[InputStream],closeFunction:() => Unit) : Seekable = {
      val dest = openResource(CloseAction.Noop)
      dest write (source.bytes)
      dest
  }

}