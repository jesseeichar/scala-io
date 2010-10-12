package scalaio.test.fs

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import scalax.io.Path.AccessModes._
import org.junit.Assert._
import org.junit.{
  Test, Ignore
}
import scalax.io.ramfs.RamFileSystem

abstract class FsAccessSetTests extends Fixture  {

  @Test
  def access_set_iterate_through_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "r"
    assertEquals(List(Read), file.access.toList)
    file.access = List(Read,Write)
    assertEquals(List(Read,Write), file.access.toList)
  }
  @Test
  def access_set_can_subtract_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "rw"
    assertTrue(file.canWrite)

    file.access -= Write
    assertEquals(List(Read), file.access.toList)
    assertFalse(file.canWrite)
  }
  @Test
  def access_set_can_add_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "r"
    assertFalse(file.canWrite)
    
    file.access += Write
    assertEquals(List(Read,Write), file.access.toList)
    assertTrue(file.canWrite)
  }
  @Test
  def access_set_can_update_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "rw"
    assertTrue(file.canWrite)

    file.access(Write) = false
    assertFalse(file.canWrite)

    file.access(Write) = true
    assertTrue(file.canWrite)
  }
}