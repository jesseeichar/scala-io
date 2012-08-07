package scalax.io
package support

import java.nio.file.{Files => JFiles}
import StandardOpenOption._
import java.nio.channels.FileChannel
import java.io.{IOException, FileOutputStream, RandomAccessFile, File}
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import java.io.InputStream
import java.io.OutputStream
import scalax.io.extractor.ReadableByteChannelExtractor
import scalax.io.extractor.WritableByteChannelExtractor
import scalax.io.extractor.FileChannelExtractor
import java.nio.file.Path
import java.nio.file.Files

object FileUtils {
  def openOutputStream(jfile: Path, openOptions: Seq[OpenOption]) = {
    val newOpts = checkForCreateFull(jfile, openOptions)
    Resource.fromOutputStream({
      JFiles.newOutputStream(jfile, newOpts:_*);
    })
  }

  def openChannel(raf: RandomAccessFile, openOptions: Seq[OpenOption]): FileChannel = {
      if (openOptions contains DeleteOnClose)
        throw new UnsupportedOperationException("DeleteOnClose is not supported for RandomAccessChannels.")
      if ((openOptions contains Truncate) && (openOptions exists {
        opt => opt == Write || opt == Append
      })){
        raf.setLength(0)
      }
      if (openOptions contains Append) {
        raf.seek(raf.length)
      }

      raf.getChannel
  }

  def openChannel(jfile:File, openOptions: Seq[OpenOption]): SeekableByteChannel = {
    openChannel(jfile.toPath, openOptions)
  }
  def openChannel(path:Path, openOptions: Seq[OpenOption]): SeekableByteChannel = {
    val newOpts = checkForCreateFull(path, openOptions)
    Files.newByteChannel(path, newOpts:_*)
  }
  
  def checkForCreateFull(path:Path, openOptions: Seq[OpenOption]) = {
    val (createFull, rest) = openOptions.partition(_ == StandardOpenOption.CreateFull)
    if(createFull.nonEmpty) {
      JFiles.createDirectories(path.getParent)
      rest :+ StandardOpenOption.Create
    } else {
      rest
    }
  }

  def copy(in:InputStream, out:OutputStream) = {
    val buf = new Array[Byte](DefaultResourceContext.recommendedByteBufferSize)
    var read = in.read(buf)
    while(read > -1) {
      if(read == 0) Thread.sleep(100)
      else {
        out.write(buf,0,read)
      }
      read = in.read(buf)
    }
  }

  def copy(in:ReadableByteChannel, out:WritableByteChannel) = {
    (in,out) match {
      case (fc: FileChannel, oc) => fc.transferTo(0, Long.MaxValue, oc)
      case (ic, fc: FileChannel) => fc.transferFrom(ic, 0, Long.MaxValue)
      case _ =>
        val buf = DefaultResourceContext.createNioBuffer(None, Some(in), true)

        var read = in.read(buf)
        while (read > -1) {
          if (read == 0) Thread.sleep(100)
          else {
            buf.flip()
            out.write(buf)
          }
          buf.clear()
          read = in.read(buf)
        }
    }
  }

  def tryCopy(failureCase: => Unit)(in:Any,out:Any) = {
    /* Bug with matching prevents the code below so have to hack around the issue
    (in,out) match {
  -  case (FileChannelExtractor(in),FileChannelExtractor(out)) =>
      out.transferFrom(in, 0, Long.MaxValue)
  -  case (in: InputStream, out: OutputStream) =>
      FileUtils.copy(in, out)
  -  case (ReadableByteChannelExtractor(in), FileChannelExtractor(out)) =>
      out.transferFrom(in, 0, Long.MaxValue)
  -  case (FileChannelExtractor(in), WritableByteChannelExtractor(out)) =>
      in.transferTo(0, Long.MaxValue, out)
  -  case (out: OutputStream, InputStreamExtractor(in)) =>
      FileUtils.copy(in, out)
    case (out: WritableByteChannel, ReadableByteChannelExtractor(in)) =>
      FileUtils.copy(in, out)
    case _ => failureCase
  }*/

    val inFileChan = FileChannelExtractor.unapply(in)
  val outFileChan = FileChannelExtractor.unapply(out)
  val readableByteChan = ReadableByteChannelExtractor.unapply(in)
  val writableByteChan = WritableByteChannelExtractor.unapply(out)

  if(inFileChan.nonEmpty && outFileChan.nonEmpty) {
    outFileChan.get.transferFrom(inFileChan.get,0,Long.MaxValue)
  } else if(in.isInstanceOf[InputStream] && out.isInstanceOf[OutputStream]){
     FileUtils.copy(in.asInstanceOf[InputStream], out.asInstanceOf[OutputStream])
  } else if(readableByteChan.nonEmpty && outFileChan.nonEmpty) {
    outFileChan.get.transferFrom(readableByteChan.get, 0, Long.MaxValue)
  } else if(inFileChan.nonEmpty && writableByteChan.nonEmpty) {
    inFileChan.get.transferTo(0, Long.MaxValue, writableByteChan.get)
  } else if(out.isInstanceOf[WritableByteChannel] && readableByteChan.nonEmpty) {
    FileUtils.copy(readableByteChan.get, out.asInstanceOf[WritableByteChannel])
  } else {
    failureCase
  }
  }
}
