/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{ 
  FileInputStream, FileOutputStream, File => JFile
}
import java.net.{ URI, URL }


import resource.ManagedResource
import StandardOpenOptions._
import collection.{Traversable }
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder}

class DefaultFile(jfile:JFile, codec:Codec) extends FileOperations(codec) {

  private implicit val defaultCodec = codec

  def inputStream = IoResource.fromInputStream(new FileInputStream(jfile))
  def outputStream(openOptions: OpenOption*) = IoResource.fromOutputStream(new FileOutputStream(jfile))
  def channel( openOptions: OpenOption*) = IoResource.fromByteChannel(new FileInputStream(jfile).getChannel)
  def fileChannel(openOptions: OpenOption*) = Some(IoResource.fromFileChannel(new FileInputStream(jfile).getChannel))
  
  def withCodec(codec:Codec) = new DefaultFile(jfile, codec)

  def open[R](openOptions: Iterable[OpenOption] = List(WRITE))(action: BasicFileOperations => R): R = null.asInstanceOf[R] // TODO

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Option[R] = {
    None
  }
}
