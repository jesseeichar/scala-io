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

  def outputStream(openOptions: OpenOption*) = Resource fromOutputStream openOutput(openOptions)
  def writableByteChannel(openOptions: OpenOption*) = Resource fromWritableByteChannel openOutput(openOptions).getChannel

  def channel( openOptions: OpenOption*) = Resource fromByteChannel openOutput(openOptions).getChannel
  def fileChannel(openOptions: OpenOption*) = Some(Resource fromFileChannel openOutput(openOptions).getChannel)
 
  def withCodec(codec:Codec) = new DefaultFileOps(jfile, codec)

  private def openOutput(openOptions: Seq[OpenOption]) = {
      var append = false
      openOptions foreach {
          case APPEND => 
              append = true
          case CREATE if !jfile.exists =>
            jfile.createNewFile()
          case CREATE_FULL if !jfile.exists =>
            jfile.getParentFile.mkdirs()
            jfile.createNewFile()
          case CREATE_NEW =>
            if (jfile.exists) Path.fail(jfile+" already exists, openOption "+CREATE_NEW+" cannot be used with an existing file")
          case TRUNCATE if (openOptions contains WRITE) && (jfile.length > 0)=>
            new FileOutputStream("/tmp/xx").close()  // truncate file
            
          case _ => ()
      }
      
      if(openOptions contains DELETE_ON_CLOSE) {
          new scalax.io.resources.DeletingFileOutputStream(jfile, append)
      } else {
          new FileOutputStream(jfile, append)
      }
  }
  

  def open[R](openOptions: Seq[OpenOption] = List(WRITE))(action: BasicFileOps => R): R =  {
      null.asInstanceOf[R]
  }

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Option[R] = {
    None
  }
  
  def bytesAsInts:Iterable[Int] = null // TODO
  
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[Process] = {
    import Path.fail
    
    if(!jfile.exists) fail(jfile+" can not be executed as it does not exist")
    if(!jfile.canExecute) fail(jfile+" can not be executed as the execution access option is not set")
    
    null // TODO
  }

}
