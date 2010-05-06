/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import scalax.io._
import scalax.io.resource._

import Path.AccessModes._
import OpenOption._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder

import scalaio.test._

import java.io.{
    IOException, DataInputStream, DataOutputStream
}

class SeekableFileTest extends AbstractSeekableTests {
    
    var fixture : FileSystemFixture = _

    @Before
    def before() : Unit = fixture = new DefaultFileSystemFixture(new TemporaryFolder())

    @After
    def after() : Unit = fixture.after()

    def open(data : Option[String] = None) : Seekable = data match {
      case None => 
        fixture.text("\n").ops
      case Some(text) => 
        val path = fixture.path
        path.ops writeString text
        path.ops
    }
}