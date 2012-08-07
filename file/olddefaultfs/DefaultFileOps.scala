/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 20010-2011, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package defaultfs

import scalax.io._
import scalax.io.managed._
import StandardOpenOption._
import scalax.io.support.FileUtils._
import java.io.{FileInputStream, File => JFile}
import java.nio.channels.FileChannel
import java.nio.file.{Path => JPath, Files => JFiles}
/**
 * <b>Not part of API.</b>
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] trait DefaultFileOps {
  self : DefaultPath =>

  // TODO OpenOptions
  override def inputStream =
    Resource.fromInputStream(JFiles.newInputStream(jpath)).updateContext(fileSystem.context)

  override def outputStream(openOptions: OpenOption*) = {
      val r = openOptions match {
          case Seq() =>
              openOutputStream(jpath,ReadWrite)
          case opts if opts forall {opt => opt != Write && opt != Append} =>
              openOutputStream(jpath,openOptions :+ Write)
          case _ =>
            openOutputStream(jpath,openOptions)
      }
      r.updateContext(fileSystem.context)
  }
  override def channel(openOptions: OpenOption*) =
    Resource.fromSeekableByteChannel(JFiles.newByteChannel(jpath,openOptions:_*)).updateContext(fileSystem.context)

  override def fileChannel(openOptions: OpenOption*):Option[SeekableByteChannelResource[FileChannel]] = {
    try {
      jpath.toFile // test that it is a file and therefore has a FileChannel
      Some(Resource.fromSeekableByteChannel(openChannel(jpath,openOptions).asInstanceOf[FileChannel]).updateContext(fileSystem.context))
    } catch {
      case e:UnsupportedOperationException => None
    }
  }
    


  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false, context:ResourceContext)(block: Seekable => R): Option[R] = {
    val self = this
    fileChannel().get.acquireAndGet{ fc =>
      Option(fc.tryLock(start,size,shared)).map{_ => block(self)}
    }
  }
}
