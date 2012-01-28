package scalax.io
package processing

/**
 * ProcessorAPI for processing Byte input sources.
 * 
 * Note: ProcessorAPI[Byte] will normally parse bytes as BigEndian bytes
 */
case class ByteProcessor(base:CloseableIteratorProcessor[Byte]) 
    extends SpecificApiFactory[Byte, ByteProcessorAPI](base) {
    protected def create(iter: CloseableIterator[Byte]) = new ByteProcessorAPI(iter)
}
/**
 * An api for reading data from the a Byte input source.  Contains methods for reading shorts, longs, etc... All datatypes (like long) read from
 * this API are big-endian.  For reading the data as little-endian data types this object provides a method to create a similar API object that
 * creates the data types by interpreting the bytes little endian.
 * 
 *  @see scalax.io.processing.ProcessorAPI
 *  @see scalax.io.processing.CharProcessorAPI
 */
class ByteProcessorAPI private[processing](private[this] val iter: CloseableIterator[Byte]) extends ProcessorAPI[Byte](iter) {
  /**
   * Create an API object for reading numeric values interpretted with little-endianness. 
   */
  def littleEndianAPI = new LittleEndianAPI(this)

  /**
   * Read a big-endian short
   */
  def nextShort = Processor {
    iterator.takeIfPossible(2) match {
      case bytes if bytes.nonEmpty =>
        Some(((bytes(0) & 0xff) << 8) | (bytes(1) & 0xff))
      case _ => None
    }
  }

  /**
   * Read a big-endian int
   */
  def nextInt = Processor {
    iterator.takeIfPossible(4) match {
      case bytes if bytes.nonEmpty =>
        Some(
          (((bytes(0) & 0xff) << 24) |
            ((bytes(1) & 0xff) << 16) |
            ((bytes(2) & 0xff) << 8) |
            (bytes(3) & 0xff)))
      case _ => None
    }
  }

  /**
   * Read a big endian long
   */
  def nextLong = Processor {
    iterator.takeIfPossible(8) match {
      case bytes if bytes.nonEmpty =>
        def long(i: Int) = (bytes(i) & 0xff).asInstanceOf[Long]
        Some(
          ((long(0) << 56) |
            (long(1) << 48) |
            (long(2) << 40) |
            (long(3) << 32) |
            (long(4) << 24) |
            (long(5) << 16) |
            (long(6) << 8) |
            (long(7))))
      case _ => None
    }
  }
  /**
   * Read a big-endian float
   */
  def nextFloat = nextInt.map(java.lang.Float.intBitsToFloat)
  /**
   * Read a big-endian double
   */
  def nextDouble = nextLong.map(java.lang.Double.longBitsToDouble)
}

/**
 * The same API as ByteProcessor except that the methods returning bytes return bytes interpreted
 * little-endian
 */
class LittleEndianAPI private[processing](@inline private[this] val wrapped: ByteProcessorAPI) {

  /**
   * Read a little-endian short
   */
  def nextShort = Processor {
    wrapped.iterator.takeIfPossible(2) match {
      case bytes if bytes.nonEmpty =>
        Some(((bytes(1) & 0xff) << 8) | (bytes(0) & 0xff))
      case _ => None
    }
  }

  /**
   * Read a little-endian int
   */
  def nextInt = Processor {
      wrapped.iterator.takeIfPossible(4) match {
        case bytes if bytes.nonEmpty => 
          Some(
              (((bytes(3) & 0xff) << 24) | 
               ((bytes(2) & 0xff) << 16) | 
               ((bytes(1) & 0xff) << 8) | 
               (bytes(0) & 0xff)))
        case _ => None
      }
    }
    
  /**
   * Read a little-endian long
   */
    def nextLong = Processor {
        wrapped.iterator.takeIfPossible(8) match {
        case bytes if bytes.nonEmpty =>
          def long(i:Int) = (bytes(i) & 0xff).asInstanceOf[Long]
            Some(
                ((long(7) << 56) |
                 (long(6) << 48) |
                 (long(5) << 40) |
                 (long(4) << 32) |
                 (long(3) << 24) |
                 (long(2) << 16) |
                 (long(1) <<  8) |
                 (long(0))))
        case _ => None
        }
    }
  /**
   * Read a little-endian float
   */
  def nextFloat = nextInt.map(java.lang.Float.intBitsToFloat)
  /**
   * Read a little-endian double
   */
  def nextDouble = nextLong.map(java.lang.Double.longBitsToDouble)

}