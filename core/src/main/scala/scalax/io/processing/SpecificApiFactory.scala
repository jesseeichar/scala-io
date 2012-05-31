package scalax.io
package processing

/**
 * Wraps a CloseableIteratorProcessor, which normally produces a ProcessorAPI object, to a new Processor that
 * returns a ProcessorAPI subclass specific to a data type.  For example
 * CharProcessorAPI has the lines() method and ByteProcessorAPI have int, long, float etc... methods
 *
 * for {
 *  byteAPI <- ByteProcessorAPI(in.bytes.processor)
 *  littleEndianAPI = byteApi.littleEndianAPI
 *  littleEndianInt <- littleEndianAPI.nextInt
 * } yield littleEndianInt
 *
 *  @see scalax.io.processing.ProcessorAPI
 *  @see scalax.io.processing.CharProcessorAPI
 */
abstract class SpecificApiFactory[A,API <: ProcessorAPI[A]](base:CloseableIteratorProcessor[A]) extends Processor[API] {
  def context = base.context
  /**
   * Factory method to create the actual API object
   */
  protected def create(commonAPI: CloseableIterator[A]):API
  private[processing] def init = new Opened[API] {
    val iter = base.iter()
    def execute() = Some(create(iter))
    def cleanUp() = iter.close()
  }
}
