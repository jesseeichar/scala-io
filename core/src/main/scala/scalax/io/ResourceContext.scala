package scalax.io
import java.io.IOException
import java.nio.ByteBuffer

trait ResourceContext {
  self =>

  final val recommendedByteBufferSize = 4 * 1024
  final val recommendedCharBufferSize = 1024

  def errorHandler(exceptions: List[Throwable]): Unit = throw new ScalaIOException(exceptions)
  def descName: ResourceDescName = UnknownName()
  def byteBufferSize(dataSize: Option[Long]): Int = dataSize match {
    case Some(size) => (size min recommendedByteBufferSize).toInt
    case None => recommendedByteBufferSize
  }
  def charBufferSize(dataSize: Option[Int]): Int = dataSize match {
    case Some(size) => size min recommendedCharBufferSize
    case None => recommendedCharBufferSize
  }
  def createNioBuffer(bufferSize: Int): java.nio.ByteBuffer = ByteBuffer.allocateDirect(bufferSize)
  final def createNioBuffer(dataSize: Option[Long]): java.nio.ByteBuffer = ByteBuffer.allocateDirect(byteBufferSize(dataSize))

  def copy(
    newByteBufferSize: Option[Option[Long] => Int] = None,
    newCharBufferSize: Option[Option[Int] => Int] = None,
    newCreateNioBuffer: Option[Int => ByteBuffer] = None,
    newErrorHandler: Option[List[Throwable] => Unit] = None,
    newDescName: Option[ResourceDescName] = None) = new ResourceContext {
    override def errorHandler(exceptions: List[Throwable]): Unit = (newErrorHandler getOrElse (self.errorHandler _))(exceptions)
    override def descName: ResourceDescName = newDescName getOrElse self.descName
    override def byteBufferSize(dataSize: Option[Long]): Int = (newByteBufferSize getOrElse (self.byteBufferSize _))(dataSize)
    override def charBufferSize(dataSize: Option[Int]): Int = (newCharBufferSize getOrElse (self.charBufferSize _))(dataSize)
    override def createNioBuffer(bufferSize: Int): java.nio.ByteBuffer = newCreateNioBuffer.map(_(bufferSize)) getOrElse self.createNioBuffer(bufferSize)
  }
}

object DefaultResourceContext extends ResourceContext