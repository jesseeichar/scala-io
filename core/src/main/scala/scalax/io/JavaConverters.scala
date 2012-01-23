package scalax.io
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.RandomAccessFile
import java.io.Reader
import java.io.Writer
import java.net.URL

import scalax.io.nio.SeekableFileChannel
import java.nio.channels.{ReadableByteChannel, FileChannel, WritableByteChannel}

object JavaConverters {
  class AsInput(op: => Input) {
    /** An object to an input object */
    def asInput: Input = op
  }

  /**
   * Wrap an arbitraty object as and AsInput object allowing the object to be converted to an Input object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsInputConverter]]
   */
  implicit def asInputConverter[B](src:B)(implicit converter:AsInputConverter[B]) =
    new AsInput(converter.toInput(src))

  /**
   * Used by the [[scalax.io.Input]] object for converting an arbitrary object to an Input Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsInputConverter[-A] {
    def toInput(t:A) : Input
  }

  /**
   * contains several implementations of [[scalax.io.AsInputConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asInput and the converter will be found without the user needing to look up these classes
   */
  object AsInputConverter {

    /**
     * Converts a File to an Input object
     */
    implicit object FileConverter extends AsInputConverter[File]{
      def toInput(file: File) = Resource.fromFile(file)
    }
    /**
     * Converts a URL to an Input object
     */
    implicit object URLConverter extends AsInputConverter[URL]{
      def toInput(url: URL) = Resource.fromURL(url)
    }
    /**
     * Converts a InputStream to an Input object
     */
    implicit object InputStreamConverter extends AsInputConverter[InputStream]{
      def toInput(is: InputStream) = Resource.fromInputStream(is)
    }
    /**
     * Converts a ReadableByteChannel to an Input object
     */
    implicit object ReadableByteChannelStreamConverter extends AsInputConverter[ReadableByteChannel]{
      def toInput(channel: ReadableByteChannel) = Resource.fromReadableByteChannel(channel)
    }
    /**
     * Converts a Traversable of Ints to an Input object.  Each Int is treated as a byte
     */
    implicit object TraversableIntsAsBytesConverter extends AsInputConverter[Traversable[Int]]{
      def toInput(t: Traversable[Int]) = new Input {
        def chars(implicit codec: Codec = Codec.default) = new LongTraversable[Char] {
          val maxChars = codec.encoder.maxBytesPerChar
          lazy val chars = codec.decode(t.view.map{_.toByte}.toArray)
          def iterator: CloseableIterator[Char] = CloseableIterator(chars.iterator)
        }
        def blocks(blockSize: Option[Int] = None) = new LongTraversable[ByteBlock] {
          val concreteBlockSize = blockSize match {
            case Some(size) => size
            case None if t.hasDefiniteSize => t.size
            case None => Buffers.BufferSize
          }
          def iterator: CloseableIterator[ByteBlock] = {
            val sliding: Iterator[Seq[Int]] = t.toIterator.sliding(concreteBlockSize, concreteBlockSize)
            val blockIter = sliding.map { block =>
              new ByteBlock {
                private[this] val data = block
                def apply(i: Int) = data(i).toByte
                def size = data.size
              }
            }
            CloseableIterator(blockIter)
          }
        }

        override def bytesAsInts = new LongTraversable[Int]{
          def iterator = new CloseableIterator[Int] {
            var iter = OutputConverter.TraversableIntConverter.toBytes(t)

            final def next() = iter.next.toInt
            final def hasNext: Boolean = iter.hasNext
            def doClose() {}
          }
        }
        def bytes = new LongTraversable[Byte]{
          def iterator = new CloseableIterator[Byte] {
            var iter = OutputConverter.TraversableIntConverter.toBytes(t)

            final def next() = iter.next
            final def hasNext: Boolean = iter.hasNext
            def doClose() {}
          }
        }

        def size = Some(t.size * 4)
      }
    }
    /**
     * Converts a Traversable[Byte] to an Input object
     */
    implicit object TraversableByteConverter extends AsInputConverter[Traversable[Byte]]{
      def toInput(t: Traversable[Byte]) = new Input {
        def chars(implicit codec: Codec = Codec.default) = new LongTraversable[Char] {
          val maxChars = codec.encoder.maxBytesPerChar

          lazy val chars = codec.decode(t.toArray)

          def iterator: CloseableIterator[Char] = CloseableIterator(chars.iterator)
        }
        def blocks(blockSize: Option[Int] = None) = new LongTraversable[ByteBlock] {
          val concreteBlockSize = blockSize match {
            case Some(size) => size
            case None if t.hasDefiniteSize => t.size
            case None => Buffers.BufferSize
          }
          def iterator: CloseableIterator[ByteBlock] = {
            val sliding: Iterator[Seq[Byte]] = t.toIterator.sliding(concreteBlockSize, concreteBlockSize)
            val blockIter = sliding.map { block =>
              new ByteBlock {
                private[this] val data = block
                def apply(i: Int) = data(i)
                def size = data.size
              }
            }
            CloseableIterator(blockIter)
          }
        }

        override def bytesAsInts = new LongTraversable[Int]{
          def iterator: CloseableIterator[Int] = CloseableIterator(t.toIterator.map(_.toInt))
        }


        override def bytes = new LongTraversable[Byte]{
          def iterator: CloseableIterator[Byte] = CloseableIterator(t.toIterator)
        }

        def size = Some(t.size)
      }
    }
  }
  class AsOutput(op: => Output) {
    /** An object to an Output object */
    def asOutput: Output = op
  }

  /**
   * Wrap an arbitraty object as and AsOutput object allowing the object to be converted to an Output object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsOutputConverter]]
   */
  implicit def asOutputConverter[B](src:B)(implicit converter:AsOutputConverter[B]) =
    new AsOutput(converter.toOutput(src))
    
  /**
   * Used by the [[scalax.io.Output]] object for converting an arbitrary object to an Output Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsOutputConverter[-A] {
    def toOutput(t:A) : Output
  }
  
  /**
   * contains several implementations of [[scalax.io.AsOutputConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asOutput and the converter will be found without the user needing to look up these classes
   */
  object AsOutputConverter {
  
    /**
     * Converts a File to an Output object
     */
    implicit object FileConverter extends AsOutputConverter[File]{
      def toOutput(file: File) = Resource.fromFile(file)
    }

    /**
     * Converts a OutputStream to an Output object
     */
    implicit object OutputStreamConverter extends AsOutputConverter[OutputStream]{
      def toOutput(out: OutputStream) = Resource.fromOutputStream(out)
    }
  /**
   * Converts a WritableByteChannel to an Output object
   */
  implicit object WritableByteChannelConverter extends AsOutputConverter[WritableByteChannel]{
	  def toOutput(chan: WritableByteChannel) = Resource.fromWritableByteChannel(chan)
  }
}
  
  class AsBinaryReadChars(op: Codec => ReadChars) {
    /** An object to an ReadChars object */
    def asBinaryReadChars(implicit codec:Codec = Codec.default): ReadChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverterFromBinary]]
   */
  implicit def asReadCharsConverter[B](src:B)(implicit converter:AsBinaryReadCharsConverter[B]) =
    new AsBinaryReadChars(codec => converter.toReadChars(src,codec))

      
  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsBinaryReadCharsConverter[-A] {
    def toReadChars(t:A,codec:Codec) : ReadChars
  }
  
  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverterFromBinary]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asBinaryReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsBinaryReadCharsConverter {

    /**
     * Converts an InputStream to an ReadChars object
     */
    implicit object InputStreamConverter extends AsBinaryReadCharsConverter[InputStream]{
      def toReadChars(in: InputStream, codec:Codec) = Resource.fromInputStream(in).reader(codec)
    }

    /**
     * Converts a File to an ReadChars object
     */
    implicit object FileConverter extends AsBinaryReadCharsConverter[File]{
      def toReadChars(file: File, codec:Codec) = Resource.fromFile(file).reader(codec)
    }
    /**
     * Converts a URL to an ReadChars object
     */
    implicit object URLConverter extends AsBinaryReadCharsConverter[URL]{
      def toReadChars(url: URL, codec:Codec) = Resource.fromURL(url).reader(codec)
    }
  }

  class AsReadChars(op: => ReadChars) {
    /**An object to an ReadChars object */
    def asReadChars: ReadChars = op
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverter]]
   */
  implicit def asReadCharsConverter[B](src:B)(implicit converter:AsReadCharsConverter[B]) =
    new AsReadChars(converter.toReadChars(src))


  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsReadCharsConverter[-A] {
    def toReadChars(t:A) : ReadChars
  }

  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverter]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsReadCharsConverter{

    /**
     * Converts a File to an ReadChars object
     */
    implicit object ReaderConverter extends AsReadCharsConverter[Reader]{
      def toReadChars(reader: Reader) = Resource.fromReader(reader)
    }
    /**
     * Converts a String to a ReadChars object
     */
    implicit object TraversableStringConverter extends AsReadCharsConverter[String]{
      def toReadChars(string: String): ReadChars = TraversableCharConverter.toReadChars(string)
    }
    /**
     * Converts a Traversable[Char] to a ReadChars object
     */
    implicit object TraversableCharConverter extends AsReadCharsConverter[Traversable[Char]]{
      def toReadChars(t: Traversable[Char]): ReadChars = new ReadChars {
        def chars: LongTraversable[Char] = new LongTraversable[Char] {

          protected[io] def iterator: CloseableIterator[Char] = CloseableIterator(t.toIterator)
        }
      }
    }
  }

  class AsSeekable(op: => Seekable) {
    /** An object to an Seekable object */
    def asSeekable: Seekable = op
  }

  /**
   * Wrap an arbitraty object as and AsSeekable object allowing the object to be converted to an Seekable object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsSeekableConverter]]
   */
  implicit def asSeekableConverter[B](src:B)(implicit converter:AsSeekableConverter[B]) =
    new AsSeekable(converter.toSeekable(src))

    
  /**
   * Used by the [[scalax.io.Seekable]] object for converting an arbitrary object to an Seekable Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsSeekableConverter[-A] {
    def toSeekable(t:A) : Seekable
  }
  
  /**
   * contains several implementations of [[scalax.io.AsSeekableConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asSeekable and the converter will be found without the user needing to look up these classes
   */
  object AsSeekableConverter {
  
    /**
     * Converts a File to an Seekable object
     */
    implicit object FileConverter extends AsSeekableConverter[File]{
      def toSeekable(file: File) = Resource.fromFile(file)
    }
    /**
     * Converts a RandomAccessFile to an Seekable object
     */
    implicit object RandomAccessFileConverter extends AsSeekableConverter[RandomAccessFile]{
      def toSeekable(raf: RandomAccessFile) = Resource.fromRandomAccessFile(raf)
    }
    /**
     * Converts a FileChannel to an Seekable object
     */
    implicit object FileChannelConverter extends AsSeekableConverter[FileChannel]{
      def toSeekable(channel: FileChannel) = Resource.fromSeekableByteChannel(new SeekableFileChannel(channel))
    }
    /**
     * Converts a SeekableByteChannel to an Seekable object
     */
    implicit object SeekableByteChannelConverter extends AsSeekableConverter[SeekableByteChannel]{
      def toSeekable(channel: SeekableByteChannel) = Resource.fromSeekableByteChannel(channel)
    }
  }

  class AsBinaryWriteChars(op: (Codec) => WriteChars) {
    /** An object to an WriteChars object */
    def asBinaryWriteChars(implicit codec:Codec = Codec.default): WriteChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asWriteCharsConverter[B](src:B)(implicit converter:AsBinaryWriteCharsConverter[B]) =
    new AsBinaryWriteChars(codec => converter.toWriteChars(src,codec))
    
  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsBinaryWriteCharsConverter[-A] {
    def toWriteChars(t:A,codec:Codec) : WriteChars
  }
  
  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsBinaryWriteCharsConverter {
  
    /**
     * Converts a File to an WriteChars object
     */
    implicit object FileConverter extends AsBinaryWriteCharsConverter[File]{
      def toWriteChars(file: File,codec:Codec) = Resource.fromFile(file).writer(codec)
    }

    /**
     * Converts a OutputStream to an WriteChars object
     */
    implicit object OutputStreamConverter extends AsBinaryWriteCharsConverter[OutputStream]{
      def toWriteChars(out: OutputStream,codec:Codec) = Resource.fromOutputStream(out).writer(codec)
    }
  }

    class AsWriteChars(op: => WriteChars) {
    /** An object to an WriteChars object */
    def asWriteChars: WriteChars = op
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asWriteCharsConverter[B](src:B)(implicit converter:AsWriteCharsConverter[B]) =
    new AsWriteChars(converter.toWriteChars(src))

  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsWriteCharsConverter[-A] {
    def toWriteChars(t:A) : WriteChars
  }

  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsWriteCharsConverter {

    /**
     * Converts a File to an WriteChars object
     */
    implicit object WriterConverter extends AsWriteCharsConverter[Writer]{
      def toWriteChars(writer: Writer) = Resource.fromWriter(writer)
    }
  }

  /**************************** Converters to for creating object from _Persistent_ resources ************************/
  class AsPersistentInput(op: => Input) {
    /** An object to an input object */
    def asPersistentInput: Input = op
  }

  /**
   * Wrap an arbitrary object as an AsInput object allowing the object to be converted to an Input object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsInputConverter]]
   */
  implicit def asPersistentInputConverter[B](src:B)(implicit converter:AsPersistentInputConverter[B]) =
    new AsPersistentInput(converter.toPersistentInput(src))

  /**
   * Used by the [[scalax.io.Input]] object for converting an arbitrary object to an Input Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentInputConverter[-A] {
    def toPersistentInput(t:A) : Input
  }

  /**
   * contains several implementations of [[scalax.io.AsInputConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asInput and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentInputConverter {
    /**
     * Converts a InputStream to an Input object
     */
    implicit object PersistentInputStreamConverter extends AsPersistentInputConverter[InputStream]{
      def toPersistentInput(is: InputStream) = Resource.fromPersistentInputStream(is)
    }
    /**
     * Converts a ReadableByteChannel to an Input object
     */
    implicit object PersistentReadableByteChannelPersistentStreamConverter extends AsPersistentInputConverter[ReadableByteChannel]{
      def toPersistentInput(channel: ReadableByteChannel) = Resource.fromPersistentReadableByteChannel(channel)
    }
  }
  class AsPersistentOutput(op: => Output) {
    /** An object to an Output object */
    def asPersistentOutput: Output = op
  }

  /**
   * Wrap an arbitraty object as and AsOutput object allowing the object to be converted to an Output object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsOutputConverter]]
   */
  implicit def asOutputPersistentConverter[B](src:B)(implicit converter:AsPersistentOutputConverter[B]) =
    new AsPersistentOutput(converter.toPersistentOutput(src))

  /**
   * Used by the [[scalax.io.Output]] object for converting an arbitrary object to an Output Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentOutputConverter[-A] {
    def toPersistentOutput(t:A) : Output
  }

  /**
   * contains several implementations of [[scalax.io.AsOutputConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asOutput and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentOutputConverter {
    /**
     * Converts a OutputStream to an Output object
     */
    implicit object PersistentOutputStreamConverter extends AsPersistentOutputConverter[OutputStream]{
      def toPersistentOutput(out: OutputStream) = Resource.fromPersistentOutputStream(out)
    }
    /**
     * Converts a WritableByteChannel to an Output object
     */
    implicit object PersistentWritableByteChannelConverter extends AsPersistentOutputConverter[WritableByteChannel]{
      def toPersistentOutput(chan: WritableByteChannel) = Resource.fromPersistentWritableByteChannel(chan)
    }
  }

  class AsPersistentBinaryReadChars(op: Codec => ReadChars) {
    /** An object to an ReadChars object */
    def asPersistentBinaryReadChars(implicit codec:Codec = Codec.default): ReadChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverterFromBinary]]
   */
  implicit def asPersistentReadCharsConverter[B](src:B)(implicit converter:AsPersistentBinaryReadCharsConverter[B]) =
    new AsPersistentBinaryReadChars(codec => converter.toPersistentReadChars(src,codec))


  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentBinaryReadCharsConverter[-A] {
    def toPersistentReadChars(t:A,codec:Codec) : ReadChars
  }

  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverterFromBinary]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asBinaryReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentBinaryReadCharsConverter {

    /**
     * Converts an InputStream to an ReadChars object
     */
    implicit object PersistentInputStreamConverter extends AsPersistentBinaryReadCharsConverter[InputStream]{
      def toPersistentReadChars(in: InputStream, codec:Codec) = Resource.fromPersistentInputStream(in).reader(codec)
    }
  }

  class AsPersistentReadChars(op: => ReadChars) {
    /** An object to an ReadChars object */
    def asPersistentReadChars: ReadChars = op
  }

  /**
   * Wrap an arbitrary object as and AsReadChars object allowing the object to be converted to an ReadChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsReadCharsConverter]]
   */
  implicit def asPersistentReadCharsConverter[B](src:B)(implicit converter:AsPersistentReadCharsConverter[B]) =
    new AsPersistentReadChars(converter.toPersistentReadChars(src))


  /**
   * Used by the [[scalax.io.ReadChars]] object for converting an arbitrary object to an ReadChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentReadCharsConverter[-A] {
    def toPersistentReadChars(t:A) : ReadChars
  }

  /**
   * contains several implementations of [[scalax.io.AsReadCharsConverter]].  They will be implicitly resolved allowing
   * a user of the library to simple call A.asReadChars and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentReadCharsConverter{

    /**
     * Converts a File to an ReadChars object
     */
    implicit object PersistentReaderConverter extends AsPersistentReadCharsConverter[Reader]{
      def toPersistentReadChars(reader: Reader) = Resource.fromPersistentReader(reader)
    }
  }

  class AsPersistentSeekable(op: => Seekable) {
    /** An object to an Seekable object */
    def asPersistentSeekable: Seekable = op
  }

  /**
   * Wrap an arbitraty object as and AsSeekable object allowing the object to be converted to an Seekable object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsSeekableConverter]]
   */
  implicit def asPersistentSeekableConverter[B](src:B)(implicit converter:AsPersistentSeekableConverter[B]) =
    new AsPersistentSeekable(converter.toPersistentSeekable(src))


  /**
   * Used by the [[scalax.io.Seekable]] object for converting an arbitrary object to an Seekable Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentSeekableConverter[-A] {
    def toPersistentSeekable(t:A) : Seekable
  }

  /**
   * contains several implementations of [[scalax.io.AsSeekableConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asSeekable and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentSeekableConverter {

    /**
     * Converts a FileChannel to an Seekable object
     */
    implicit object PersistentFileChannelConverter extends AsPersistentSeekableConverter[FileChannel]{
      def toPersistentSeekable(channel: FileChannel) = Resource.fromPersistentSeekableByteChannel(new SeekableFileChannel(channel))
    }
    /**
     * Converts a SeekableByteChannel to an Seekable object
     */
    implicit object PersistentSeekableByteChannelConverter extends AsPersistentSeekableConverter[SeekableByteChannel]{
      def toPersistentSeekable(channel: SeekableByteChannel) = Resource.fromPersistentSeekableByteChannel(channel)
    }
  }

  class AsPersistentBinaryWriteChars(op: (Codec) => WriteChars) {
    /** An object to an WriteChars object */
    def asPersistentBinaryWriteChars(implicit codec:Codec = Codec.default): WriteChars = op(codec)
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asPersistentWriteCharsConverter[B](src:B)(implicit converter:AsPersistentBinaryWriteCharsConverter[B]) =
    new AsPersistentBinaryWriteChars(codec => converter.toPersistentWriteChars(src,codec))

  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentBinaryWriteCharsConverter[-A] {
    def toPersistentWriteChars(t:A,codec:Codec) : WriteChars
  }

  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentBinaryWriteCharsConverter {
    /**
     * Converts a OutputStream to an WriteChars object
     */
    implicit object PersistentOutputStreamConverter extends AsPersistentBinaryWriteCharsConverter[OutputStream]{
      def toPersistentWriteChars(out: OutputStream,codec:Codec) = Resource.fromPersistentOutputStream(out).writer(codec)
    }
  }

  class AsPersistentWriteChars(op: => WriteChars) {
    /**An object to an WriteChars object */
    def asPersistentWriteChars: WriteChars = op
  }

  /**
   * Wrap an arbitrary object as and AsWriteChars object allowing the object to be converted to an WriteChars object.
   *
   * The possible types of src are the subclasses of [[scalax.io.AsWriteCharsConverter]]
   */
  implicit def asPersistentWriteCharsConverter[B](src:B)(implicit converter:AsPersistentWriteCharsConverter[B]) =
    new AsPersistentWriteChars(converter.toPersistentWriteChars(src))

  /**
   * Used by the [[scalax.io.WriteChars]] object for converting an arbitrary object to an WriteChars Object
   *
   * Note: this is a classic use of the type class pattern
   */
  trait AsPersistentWriteCharsConverter[-A] {
    def toPersistentWriteChars(t:A) : WriteChars
  }

  /**
   * contains several implementations of [[scalax.io.AsWriteCharsConverter]].  They will be implicitely resolved allowing
   * a user of the library to simple call A.asWriteChars and the converter will be found without the user needing to look up these classes
   */
  object AsPersistentWriteCharsConverter {

    /**
     * Converts a File to an WriteChars object
     */
    implicit object PersistentWriterConverter extends AsPersistentWriteCharsConverter[Writer]{
      def toPersistentWriteChars(writer: Writer) = Resource.fromPersistentWriter(writer)
    }
  }
}