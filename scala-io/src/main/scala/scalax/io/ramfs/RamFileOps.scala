/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.{
  FileOps, OpenOption, Seekable, NotFileException
}
import scalax.io.resource.ByteChannelResource
import java.nio.channels.FileChannel
import java.io.{
  FileNotFoundException, IOException
}

class RamFileOps(path:RamPath) extends FileOps(path) {
  private def fileResource[A](toResource:FileNode => A, openOptions: OpenOption*) = {
    val options = if(openOptions.isEmpty) OpenOption.WriteTruncate
                    else openOptions
                    
    import OpenOption._
    if((options contains CreateNew) && path.exists) {
      throw new IOException(path+" exists and therefore cannot be created new");
    }
    
    
    path.fileSystem.lookup(path) match {
      case Some(file:FileNode) => 
        toResource(file)
      case Some(_) => 
        throw new NotFileException()
      case None if options exists {e => List(CreateNew, Create) contains e} =>
        path.createFile(false)
        path.fileSystem.lookup(path).collect {case f:FileNode => toResource(f)}.getOrElse {error("file should have been created")}
      case None if options contains CreateFull =>
        path.createFile()
        path.fileSystem.lookup(path).collect {case f:FileNode => toResource(f)}.getOrElse {error("file should have been created because of open option createFull")}
      case None => 
        throw new FileNotFoundException("No file found and will not create when open options are: " + openOptions.size)
    }
    
  }
  def inputStream = fileResource(_.inputResource,OpenOption.Read)
  def outputStream(openOptions: OpenOption*) = fileResource( _.outputResource(path, openOptions:_*), openOptions:_*)
  def channel(openOptions: OpenOption*) = fileResource(_.channel(path, openOptions:_*), openOptions:_*)
  def fileChannel(openOptions: OpenOption*) : Option[ByteChannelResource[FileChannel]] = None // not supported

  def withLock[R](start: Long,size: Long,shared: Boolean)(block: (Seekable) => R):Option[R] = None // TODO
  def open[R](openOptions: Seq[OpenOption])(action: (Seekable) => R):R = null.asInstanceOf[R] // TODO
}
