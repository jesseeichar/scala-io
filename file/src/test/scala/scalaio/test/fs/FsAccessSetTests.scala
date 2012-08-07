package scalaio.test.fs

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import scalax.file.Path.AccessModes._
import scalax.file._
import org.junit.Assert._
import org.junit.Test
import scalax.test.sugar.AssertionSugar
import scalax.file.defaultfs.DefaultPath
import collection.JavaConverters._
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.FileTime

abstract class FsAccessSetTests extends Fixture {

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

  def assertEqualAttribute[T](value: T, att: String)(implicit file: Path) = {
    val attValue = file.attributes(att) getOrElse { throw new AssertionError(att + " was not one of " + file.attributes) }
    assertEquals(value, attValue)
  }

  def assertReadOnly(isReadOnly: Boolean)(implicit file: Path) = {
    if (file.attributes.supportsView[DosFileAttributeView]) {
      assertEqualAttribute(isReadOnly, "read-only")
    }
    if (file.attributes.supportsView[PosixFileAttributeView]) {
      val perm = if (isReadOnly) "r--r--r--" else "rw-rw-rw-"
      assertEqualAttribute(perm, "posix:permissions")
    }
  }
  

  @Test
  def attributes_can_read_access {
    implicit val file = fixture.path.createFile()

    assertEqualAttribute(file.lastModified, "lastModified")
    file.access = "rw"
    assertReadOnly(false)
    
    file.access -= Write
    assertReadOnly(true)

    file.access += Write
    assertReadOnly(false)

    val newTime = 1324046126000L;
    file.lastModified = newTime
    assertEqualAttribute(newTime, "lastModified")
    assertEquals(newTime, file.lastModified)
  }

  @Test
  def setting_attributes_can_update {
    implicit val file = fixture.path.createFile()

	if (file.attributes.supportsView[PosixFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("posix:permissions", "rw-rw-rw-"))
	} else if (file.attributes.supportsView[DosFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("read-only", false))
    }
    
    assertReadOnly(false)

	if (file.attributes.supportsView[PosixFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("posix:permissions", "r--r--r--"))
	} else if (file.attributes.supportsView[DosFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("read-only", true))
    }

    assertReadOnly(true)
    
    if (file.attributes.supportsView[PosixFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("posix:permissions", "rw-rw-rw-"))
	} else if (file.attributes.supportsView[DosFileAttributeView]) {
	  file.attributes = Set(FileAttributeImpl("read-only", false))
    }

    assertReadOnly(false)

    if (file.attributes.supportsView[PosixFileAttributeView]) {
	  file.attributes.view[PosixFileAttributeView].get.setPermissions(PosixFilePermissions.fromString("r--r--r--"))
	} else if (file.attributes.supportsView[DosFileAttributeView]) {
	  file.attributes.view[DosFileAttributeView].get.setReadOnly(true)
    }

    assertReadOnly(true)
    
    if (file.attributes.supportsView[PosixFileAttributeView]) {
    	file.attributes.view[PosixFileAttributeView].get.setPermissions(PosixFilePermissions.fromString("rw-rw-rw-"))
    } else if (file.attributes.supportsView[DosFileAttributeView]) {
    	file.attributes.view[DosFileAttributeView].get.setReadOnly(false)
    }
    
    assertReadOnly(false)

    

    val newTime = FileTime.fromMillis(1324046126000L);
    file.attributes.view[BasicFileAttributeView].get.setTimes(newTime, newTime, newTime)

    assertEqualAttribute(newTime, "lastModified")
    assertEquals(newTime, file.lastModified)
  }
}
