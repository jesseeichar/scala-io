/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.net._
import ramfs.RamFileSystem

/**
 * Adds support to for the filesystem URL protocols.
 *
 * This handler factory can be used as a replacement for the default handler factory 
 * by calling PathURLStreamHandlerFactory.install.  This will attempt to call
 * URL.setURLStreamHandlerFactory using the PathURLStreamHandlerFactory as the parameter
 *
 * This does not always work however because the URL.setURLStreamHandlerFactory method
 * can only be called once per JVM.  For example Tomcat calls that method on startup
 * and therefore it cannot called when using tomcat.
 *
 * A second way to be able to create URLs that can open connections to custom Path
 * implementations is to create the URLs using:
 * 
 * new URL(null,"protocol://path",new PathURLStreamHandler())
 *
 * The final and perhaps best way (if possible) is to start the JVM with the
 * parameter -Djava.protocol.handler.pkgs=scalax.io
 * This will instruct the JVM to look for protocol handlers provided by scalaio
 */
object PathURLStreamHandlerFactory extends URLStreamHandlerFactory {
  def install = URL.setURLStreamHandlerFactory(this)
  
  val supported = List(RamFileSystem.protocol)
  
  def createURLStreamHandler(protocol:String) = {
    println("scalax.io."+protocol+".Handler")
    if(supported contains protocol) Class.forName("scalax.io."+protocol+".Handler").newInstance.asInstanceOf[URLStreamHandler]
    else null
  }
}

