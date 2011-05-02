/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 20010-2011, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs

import scalax.io._
import StandardOpenOption._
import scalax.io.nio.SeekableFileChannel


import scalax.io.support.DeletingFileOutputStream
import scalax.io.support.FileUtils._
import java.io.{OutputStream, FileInputStream, FileOutputStream, File => JFile, RandomAccessFile}
import java.nio.channels.Channels
import scalax.io.CloseAction.Noop

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
              openOutputStream(jfile,openOptions)
          case opts if opts forall {opt => opt != Write && opt != Append} =>
              openOutputStream(jfile,openOptions :+ Write)
          case _ =>
            openOutputStream(jfile,openOptions)
      }
  }

  def channel(openOptions: OpenOption*) = {
    Resource fromByteChannel openChannel(jfile,openOptions)
  }
  def fileChannel(openOptions: OpenOption*) = Some(Resource fromByteChannel openChannel(jfile,openOptions).self)

  def open[R](openOptions: Seq[OpenOption] = List(Read,Write))(action: OpenSeekable => R): R = {
    val c = openChannel(jfile,openOptions)
    val path = this
    val seekable = new Seekable {
      def size = path.size
      def position:Long = c.position
      def position_=(position:Long):Unit = {c.position(position)}

      override def open[U](f: (OpenSeekable) => U): U = f(this)
      protected def underlyingChannel(append: Boolean) = new OpenedResource[SeekableFileChannel] {
        val get = new SeekableFileChannel(c.self){
          override def close = {}
        }

        def close(): List[Throwable] = Nil
      }
    }
    try {
      seekable.open(action)
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
}
