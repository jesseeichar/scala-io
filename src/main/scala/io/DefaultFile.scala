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

  lazy val inputStream = InputStreamResource(new FileInputStream(jfile))
  lazy val outputStream = OutputStreamResource(new FileOutputStream(jfile))
  lazy val channel = ByteChannelResource(new FileInputStream(jfile).getChannel)
  lazy val fileChannel = Some(FileChannelResource(new FileInputStream(jfile).getChannel))
  
  def withCodec(codec:Codec): File = new DefaultFile(jfile, codec)


  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Some[R] = {
    // TODO
    null
  }
  
}
