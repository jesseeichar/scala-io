/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 20010-2011, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs

import scalax.io.StandardOpenOption
import StandardOpenOption._
import StandardOpenOption.{name => _}
import scalax.io.nio.SeekableFileChannel


import java.io.{
  FileInputStream, FileOutputStream, File => JFile, RandomAccessFile
}
import scalax.io.{OpenOption, Seekable, Resource}
import scalax.io.support.DeletingFileOutputStream


/**
 * <b>Not part of API.</b>
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] trait DefaultFileOps {
  self : DefaultPath =>

  def inputStream = Resource.fromInputStream(new FileInputStream(jfile))

  def outputStream(openOptions: OpenOption*) = {
      openOptions match {
          case Seq() =>
              openOutputStream(openOptions)
          case opts if opts forall {opt => opt != Write && opt != Append} =>
              openOutputStream(openOptions :+ Write)
          case _ =>
            openOutputStream(openOptions)
      }
  }

  def channel(openOptions: OpenOption*) = {
    val channel = new SeekableFileChannel(openChannel(openOptions))
    Resource fromByteChannel channel
  }
  def fileChannel(openOptions: OpenOption*) = Some(Resource fromByteChannel openChannel(openOptions))

  def open[R](openOptions: Seq[OpenOption] = List(Read,Write))(action: Seekable => R): R = {
    val c = openChannel(openOptions)
    val path = this
    val seekable = new Seekable {
      def size = path.size
      protected def channel(openOptions:OpenOption*) = {
        val seekable2 = new SeekableFileChannel(c) {
            override def close = () // will close after all operations
          }
        Resource fromByteChannel seekable2
      }
    }

    try {
      action(this)
    } finally {
      c.close()
    }
  }

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: Seekable => R): Option[R] = {
    val self = this
    fileChannel().get.acquireAndGet{ fc =>
      Option(fc.tryLock(start,size,shared)).map{_ => block(self)}
    }
  }

  private def preOpen(openOptions: Seq[OpenOption]) : (Boolean, Seq[OpenOption]) = {
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
            if (jfile.exists) Path.fail(jfile+" already exists, openOption "+CreateNew+" cannot be used with an existing file")
          case Truncate if (openOptions contains Write) && (jfile.length > 0)=>
            new FileOutputStream(jfile).close()  // truncate file

          case _ => ()
      }

      (append,options)
  }

  private def openOutputStream(openOptions: Seq[OpenOption]) = {
    val (append, options) = preOpen(openOptions)
    if (options contains DeleteOnClose) {
        Resource fromOutputStream new DeletingFileOutputStream(jfile, append)
    } else {
        Resource fromOutputStream new FileOutputStream(jfile,append)
    }
  }

  private def openChannel(openOptions: Seq[OpenOption]) = {
    val (_, options) = preOpen(openOptions)
    if (options contains DeleteOnClose) {
       throw new UnsupportedOperationException("DeleteOnClose is not supported on FileChannels pre Java 7 implementations.")
    } else {
        randomAccessFile(options)
    }
  }

  private def randomAccessFile(openOptions: Seq[OpenOption]) = {
      val unsortedChars = openOptions collect {
          case Write | Append => 'w'
          case Read => 'r'
          case Sync => 's'
          case DSync => 'd'
      }


      // only values acceptable to RandomAccessFile are r rw rwd or rws
      // so need to do some massaging
      val sortedChars = unsortedChars.distinct.sortWith{ (x,y) =>
          def value(x : Char) = x match {
              case 'r' => 0
              case 'w' => 1
              case 's' => 2
              case 'd' => 3
          }
          value(x) < value(y)
      }

      val chars = if(sortedChars.mkString endsWith "sd") sortedChars.takeWhile(_ != 's')
                  else sortedChars

      val file = if(chars contains 'r') {
          new RandomAccessFile(jfile, chars mkString)
      } else {
          new RandomAccessFile(jfile, 'r' + chars.mkString)
      }

      if(openOptions contains Append) file.seek(file.length)

      file.getChannel
  }
}
