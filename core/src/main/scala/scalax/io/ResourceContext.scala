package scalax.io
import java.io.IOException

object ResourceContext {
  val recommendedByteBufferSize = 4 * 1024
  val recommendedCharBufferSize = 1024

  val byteBufferSize: Option[Long] => Int = (dataSize: Option[Long]) => dataSize match {
    case Some(size) => (size min recommendedByteBufferSize).toInt
    case None => recommendedByteBufferSize
  }
  val charBufferSize: Option[Int] => Int = (dataSize: Option[Int]) => dataSize match {
    case Some(size) => size min recommendedCharBufferSize
    case None => recommendedCharBufferSize
  }
  val errorHandler: List[Throwable] => Unit = (exceptions: List[Throwable]) => throw new ScalaIOException(exceptions)
}
case class ResourceContext (
  recommendedByteBufferSize:Int = ResourceContext.recommendedByteBufferSize,
  recommendedCharBufferSize:Int = ResourceContext.recommendedCharBufferSize,
  byteBufferSize: Option[Long] => Int = ResourceContext.byteBufferSize,
  charBufferSize: Option[Int] => Int = ResourceContext.charBufferSize,
  errorHandler: List[Throwable] => Unit = ResourceContext.errorHandler,
  descName:ResourceDescName = UnknownName()) {
  
  //def updateCloseAction(f:)
}
