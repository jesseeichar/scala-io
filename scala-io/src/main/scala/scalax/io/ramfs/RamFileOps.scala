/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.{
  Path, FileOps, OpenOption, Seekable, SeekableByteChannel, NotFileException
}
import scalax.io.resource.{
  ByteChannelResource, OutputStreamResource, InputStreamResource, Resource
}
import java.nio.channels.FileChannel
import java.io.{
  InputStream, OutputStream, FileNotFoundException, ByteArrayInputStream, ByteArrayOutputStream
}

class RamFileOps(path:RamPath) extends FileOps(path) {
  private def fileResource[A](toResource:FileNode => A) = {
    path.fileSystem.lookup(path) match {
      case Some(file:FileNode) => 
        toResource(file)
      case Some(_) => 
        throw new NotFileException()
      case None => 
        throw new FileNotFoundException()
    }
    
  }
  def inputStream = fileResource(_.inputResource)
  def outputStream(openOptions: OpenOption*) = fileResource(_.outputResource(openOptions:_*))
  def fileChannel(openOptions: OpenOption*) : Option[ByteChannelResource[FileChannel]] = None // not supported
  def channel(openOptions: OpenOption*) = fileResource(_.channel)

  def withLock[R](start: Long,size: Long,shared: Boolean)(block: (Seekable) => R):Option[R] = None // TODO
  def open[R](openOptions: Seq[OpenOption])(action: (Seekable) => R):R = null.asInstanceOf[R] // TODO
}
