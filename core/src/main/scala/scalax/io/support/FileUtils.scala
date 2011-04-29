package scalax.io
package support

import StandardOpenOption._
import java.nio.channels.FileChannel
import nio.SeekableFileChannel
import java.io.{IOException, FileOutputStream, RandomAccessFile, File}

object FileUtils {
  def openOutputStream(jfile:File, openOptions: Seq[OpenOption]) = {
    val (append, options) = preOpen(jfile,openOptions,true)
    if (options contains DeleteOnClose) {
        Resource fromOutputStream new DeletingFileOutputStream(jfile, append)
    } else {
        Resource fromOutputStream new FileOutputStream(jfile,append)
    }
  }
  def openChannel(raf:RandomAccessFile, openOptions: Seq[OpenOption]):SeekableFileChannel = {
    if (openOptions contains DeleteOnClose)
      throw new UnsupportedOperationException("DeleteOnClose is not supported on FileChannels pre Java 7 implementations.")
    if((openOptions contains Truncate) && (openOptions exists {opt => opt == Write || opt == Append}))
      raf.setLength(0)
    if(openOptions contains Append)
      raf.seek(raf.length)

    new SeekableFileChannel(raf.getChannel)

  }

  def openChannel(jfile:File, openOptions: Seq[OpenOption]):SeekableFileChannel= {
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
          jfile.getParentFile.mkdirs()
          jfile.createNewFile()
        case CreateNew =>
          if (jfile.exists) throw new IOException(jfile+" already exists, openOption "+CreateNew+" cannot be used with an existing file")
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
}