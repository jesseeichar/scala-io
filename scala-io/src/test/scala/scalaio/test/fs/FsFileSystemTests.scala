/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import Path.AccessModes._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.IOException

abstract class FsFileSystemTests extends scalax.test.sugar.AssertionSugar with Fixture{
    implicit val codec = Codec.UTF8

    @Test
    def fileSystem_apply_creates_a_path() : Unit = {
        val path = FileSystem.default(getClass.getClassLoader.getResource("resources/text").getFile)
        assertTrue(path.exists)
        assertTrue(path.canRead)
    }
}