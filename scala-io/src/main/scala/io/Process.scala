/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.lang.{ Process => JProcess }

/**
 * A wrapper around java.lang.Process making it more idiomatic of Scala
 * <p>
 * Not done !
 * </p>
 * 
 * @see java.lang.Process
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
class Process(process: JProcess) {
  def destroy(): Unit = process.destroy()
  def exitValue(): Int = process.exitValue()
//  def errorStream: ManagedResource[InputStream] = 
//  def inputStream: ManagedResource[InputStream] = 
//  def outputStream: ManagedResource[OutputStream] = 

}
