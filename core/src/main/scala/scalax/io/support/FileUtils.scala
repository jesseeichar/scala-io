package scalax.io
package support

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

object FileUtils {
  def openOutputStream(jfile: File, openOptions: Seq[OpenOption]) = {
    Resource.fromOutputStream({
      val (append, options) = preOpen(jfile, openOptions, true)
      if (options contains DeleteOnClose) {
        new DeletingFileOutputStream(jfile, append)
      } else {
        new FileOutputStream(jfile, append)
      }
    })
  }

  def openChannel(raf: RandomAccessFile, openOptions: Seq[OpenOption]): FileChannel = {
      if (openOptions contains DeleteOnClose)
        throw new UnsupportedOperationException("DeleteOnClose is not supported on FileChannels pre Java 7 implementations.")
      if ((openOptions contains Truncate) && (openOptions exists {
        opt => opt == Write || opt == Append
      }))
        raf.setLength(0)
      if (openOptions contains Append)
        raf.seek(raf.length)

      raf.getChannel
  }

  def openChannel(jfile:File, openOptions: Seq[OpenOption]): FileChannel= {
    val (_, options) = preOpen(jfile,openOptions,false)
    if (options contains DeleteOnClose) {
     throw new UnsupportedOperationException("DeleteOnClose is not supported on FileChannels pre Java 7 implementations.")
    } else {
      openChannel(randomAccessFile(jfile,options),options)
    }
  }

  private def preOpen(jfile:File, openOptions: Seq[OpenOption],processTruncate:Boolean) : (Boolean, Seq[OpenOption]) = {
     val options = if(openOptions.isEmpty) WriteTruncate
                    else openOptions

      var append = false
      options foreach {
        case Append =>
          append = true
        case Create if !jfile.exists =>
          jfile.createNewFile()
        case CreateFull if !jfile.exists =>
          var parent = Option(jfile.getParentFile) orElse Option(jfile.getAbsoluteFile().getParentFile())
          parent.getOrElse(throw new IOException("unable to get parent file of"+jfile)).mkdirs()
          jfile.createNewFile()
        case CreateNew =>
          if (jfile.exists)
            throw new IOException(jfile+" already exists, openOption '"+CreateNew+"' cannot be used with an existing file")
        case Truncate if processTruncate && (options.exists {opt => opt == Write || opt == Append} && (jfile.length > 0))=>
          new FileOutputStream(jfile).close()  // truncate file

        case _ => ()
    }

    (append,options)
  }

  private def randomAccessFile(jfile:File, openOptions: Seq[OpenOption]) = {
    val unsortedChars = openOptions collect {
      case Write | Append => 'w'
      case Read => 'r'
      case Sync => 's'
      case DSync => 'd'
    }


    // only values acceptable to RandomAccessFile are r rw rwd or rws
    // so need to do some massaging
    val sortedChars = unsortedChars.distinct.sortWith {
      (x, y) =>
        def value(x: Char) = x match {
          case 'r' => 0
          case 'w' => 1
          case 's' => 2
          case 'd' => 3
        }
        value(x) < value(y)
    }

    val chars = if (sortedChars.mkString endsWith "sd") sortedChars.takeWhile(_ != 's')
    else sortedChars

    if (chars contains 'r') {
      new RandomAccessFile(jfile, chars mkString)
    } else {
      new RandomAccessFile(jfile, 'r' + chars.mkString)
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
