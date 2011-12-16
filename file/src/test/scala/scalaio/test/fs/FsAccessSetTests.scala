package scalaio.test.fs

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import scalax.file.Path.AccessModes._
import org.junit.Assert._
import org.junit.Test
import scalax.file.attributes._

abstract class FsAccessSetTests extends Fixture  {

  @Test
  def access_set_iterate_through_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "r"
    assertEquals(permissions(Read), file.access.toSet)
    
    file.access = List(Read,Write)
    assertEquals(permissions(Read,Write), file.access.toSet)
  }
  @Test
  def access_set_can_subtract_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "rw"
    assertTrue(file.canWrite)
    
    file.access -= Write
    assertEquals(permissions(Read), file.access.toSet)
    assertFalse(file.canWrite)
  }
  @Test
  def access_set_can_add_access : Unit = {
    val file = fixture.path.createFile()
    file.access = "r"
    assertFalse(file.canWrite)

    file.access += Write
    assertEquals(permissions(Read,Write), file.access.toSet)
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
  
  @Test
  def attributes_can_read_access {
    val file = fixture.path.createFile()
    
    def assertEqualAttribute[T, U <: FileAttribute[T]](value:T, att: Class[U]) = {
      val attValue = file.attributes find {a => att.isAssignableFrom(a.getClass)} getOrElse {throw new AssertionError(att+" was not one of "+file.attributes)}
      assertEquals(value, attValue.value)
    }

    file.access = "rw"
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(true, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])

    file.access -= Write
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(false, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])
    
    file.access += Write
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(true, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])
    
    val newTime = 1324046126000L;
     file.lastModified = newTime
    assertEqualAttribute(newTime, classOf[LastModifiedAttribute])
    assertEquals(newTime, file.lastModified)
  }
  
  @Test
  def setting_attributes_can_update {
    val file = fixture.path.createFile()
    def assertEqualAttribute[T, U <: FileAttribute[T]](value:T, att: Class[U]) = {
      val attValue = file.attributes find {a => att.isAssignableFrom(a.getClass)} getOrElse {throw new AssertionError(att+" was not one of "+file.attributes)}
      assertEquals(value, attValue.value)
    }

    file.attributes = List(WriteAccessAttribute(true), ReadAccessAttribute(true))
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(true, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])

    file.attributes = List(WriteAccessAttribute(false))
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(false, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])
    
    file.attributes = List(WriteAccessAttribute(true))
    assertEqualAttribute(true, classOf[ReadAccessAttribute])
    assertEqualAttribute(true, classOf[WriteAccessAttribute])
    assertEqualAttribute(false, classOf[ExecuteAccessAttribute])
    assertEqualAttribute(file.lastModified, classOf[LastModifiedAttribute])
    
    val newTime = 1324046126000L;
    file.attributes = List(LastModifiedAttribute(newTime))
    assertEqualAttribute(newTime, classOf[LastModifiedAttribute])
    assertEquals(newTime, file.lastModified)
  }
}
