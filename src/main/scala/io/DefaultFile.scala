/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.resource.ManagedResource
import java.io.{ 
  FileInputStream, FileOutputStream, File => JFile
}
import java.net.{ URI, URL }
import collection.{Traversable }
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder}

class DefaultFile(jfile:JFile, codec:Codec) extends File(codec) {

  def inputStream = IoResource.inputStream(new FileInputStream(jfile))
  def outputStream( openOptions: OpenOptions*) = IoResource.outputStream(new FileOutputStream(jfile))
  def channel( openOptions: OpenOptions*) = IoResource.byteChannel(new FileInputStream(jfile).getChannel)
  def fileChannel(openOptions: OpenOptions*) = Some(IoResource.fileChannel(new FileInputStream(jfile).getChannel))
  
  def withCodec(codec:Codec): File = new DefaultFile(jfile, codec)

  def open[R](openOptions: Iterable[OpenOptions] = List(StandardOpenOptions.WRITE))(action: => R): Unit = null // TODO

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Some[R] = {
    // TODO
    null
  }
  
}
