/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.file.PathURLStreamHandlerFactory
import org.junit.Assert._
import org.junit.Test
import scalax.file.ramfs.RamFileSystem

class URLStreamHandlerFactoryTest {
  @Test
  def testDefaultFactories :Unit = {
    assertNotNull(PathURLStreamHandlerFactory createURLStreamHandler RamFileSystem.protocol)
    // the following should return null so the default handlers will be used
    assertNull(PathURLStreamHandlerFactory createURLStreamHandler "file")
    assertNull(PathURLStreamHandlerFactory createURLStreamHandler "http")
  }
}
