/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs
import java.net._
import java.lang.Class

class Handler extends URLStreamHandler {
  def openConnection(url:URL) = {
    require(url.getProtocol == RamFileSystem.protocol)
    require(url.getHost contains "!")
    
    new RamURLConnection(url)
  }
}

object Handler extends Handler

class RamURLConnection(url:URL) extends URLConnection(url) {
  lazy val path = {
    val Array(id,path) = url.toString.drop(RamFileSystem.protocol+"://" size).split("!")
    RamFileSystem(id)(path)
  }
  def connect = {}

  override def getInputStream = {
    require(getDoInput, "getDoInput must be true")
    path.ops.inputStream.open()
  }

  override def getOutputStream = {
    require(getDoOutput, "getDoOutput must be true")
    path.ops.outputStream().open()
  }

  override def getLastModified = path.lastModified

  override def getContentLength = path.length.toInt
}