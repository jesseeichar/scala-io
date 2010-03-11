/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalax.io.resource._
import java.io.{ 
  FileInputStream, FileOutputStream, File => JFile
}
import java.net.{ URI, URL }


import scala.resource.ManagedResource
import OpenOption._
import collection.{Traversable }
import PartialFunction._
import util.Random.nextASCIIString
import java.lang.{ProcessBuilder}
/**
 * <b>Not part of API.</b>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
private[io] class DefaultFileOps(jfile:JFile, codec:Codec) extends FileOps(codec) {

  private implicit val defaultCodec = codec

  def inputStream = Resource.fromInputStream(new FileInputStream(jfile))
  def readableByteChannel = Resource.fromReadableByteChannel(new FileInputStream(jfile).getChannel())

  def outputStream(openOptions: OpenOption*) = Resource.fromOutputStream(new FileOutputStream(jfile))
  def writableByteChannel(openOptions: OpenOption*) = Resource.fromWritableByteChannel(new FileInputStream(jfile).getChannel())

  def channel( openOptions: OpenOption*) = Resource.fromByteChannel(new FileInputStream(jfile).getChannel)
  def fileChannel(openOptions: OpenOption*) = Some(Resource.fromFileChannel(new FileInputStream(jfile).getChannel))
  
  def withCodec(codec:Codec) = new DefaultFileOps(jfile, codec)

  def open[R](openOptions: Iterable[OpenOption] = List(WRITE))(action: BasicFileOps => R): R = null.asInstanceOf[R] // TODO

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Option[R] = {
    None
  }
  
  def bytesAsInts:Iterable[Int] = null // TODO
  
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[Process] = {
    import Path.fail
    
    if(!jfile.exists) fail(path+" can not be executed as it does not exist")
    if(!jfile.canExecute) fail(path+" can not be executed as the execution access option is not set")
    
    null // TODO
  }
}
