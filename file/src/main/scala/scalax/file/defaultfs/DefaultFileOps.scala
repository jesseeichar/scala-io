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
import scalax.io.nio.SeekableFileChannel
import scalax.io.support.FileUtils._
import java.io.{FileInputStream, File => JFile}

/**
 * <b>Not part of API.</b>
 *
 * @author  Jesse Eichar
 * @since   1.0
 */
private[file] trait DefaultFileOps {
  self : DefaultPath =>

  override def inputStream =
    Resource.fromInputStream(new FileInputStream(jfile)).updateContext(fileSystem.context)

  override def outputStream(openOptions: OpenOption*) = {
      val r = openOptions match {
          case Seq() =>
              openOutputStream(jfile,ReadWrite)
          case opts if opts forall {opt => opt != Write && opt != Append} =>
              openOutputStream(jfile,openOptions :+ Write)
          case _ =>
            openOutputStream(jfile,openOptions)
      }
      r.updateContext(fileSystem.context)
  }
  override def channel(openOptions: OpenOption*) =
    Resource.fromSeekableByteChannel(openChannel(jfile,openOptions)).updateContext(fileSystem.context)

  override def fileChannel(openOptions: OpenOption*):Some[SeekableByteChannelResource[SeekableFileChannel]] =
    Some(Resource.fromSeekableByteChannel(openChannel(jfile,openOptions)).updateContext(fileSystem.context))


  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false, context:ResourceContext)(block: Seekable => R): Option[R] = {
    val self = this
    fileChannel().get.acquireAndGet{ fc =>
      Option(fc.self.tryLock(start,size,shared)).map{_ => block(self)}
    }
  }
}
