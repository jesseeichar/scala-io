/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalax.file._
import PathMatcher._
import Path.AccessModes._
import org.junit.Test
import org.junit.Assert._
import java.nio.file.attribute.DosFileAttributeView
import scalax.file.FileAttributeImpl
import java.util.concurrent.TimeUnit

abstract class FsMatchingTests extends scalax.test.sugar.AssertionSugar with Fixture {
  implicit val codec = Codec.UTF8

  @Test //@Ignore
  def file = {
    val path = fixture.path
    path.createFile()
    val IsFile(f) = path // will throw MatchError is not a file
    intercept[MatchError] {val IsDirectory(x) = path}
  }

  @Test //@Ignore
  def dir = {
    val path = fixture.path
    path.createDirectory()
    val IsDirectory(d) = path // will throw MatchError is not a dir
    intercept[MatchError] {val IsFile(x) = path}
  }

  @Test //@Ignore
  def existance = {
    val path = fixture.path

    val NonExistent(p) = path // will throw MatchError file exists
    intercept[MatchError] {val Exists(x) = path}

    path.createFile()

    val Exists(p2) = path // will throw MatchError if does not exist
    intercept[MatchError] {val NonExistent(x) = path}
  }


   @Test //@Ignore
   def name = {
     val path = fixture.path
     path.createDirectory()
     val NameIsXXX = NameIs(path.name)
     val NameIsXXX(d) = path
   }


  @Test //@Ignore
  def accessMatcher = {
    val path = fixture.path
    path.createFile()
    def test (enabled:Iterable[AccessMode], disabled:Iterable[AccessMode]) = {
      path.access = enabled
      enabled.foreach{p => assert(AccessMatcher(p).unapply(path).isDefined, "Expected '"+p+"' but was not found with matcher")}
      disabled.foreach{p => assert(AccessMatcher(p).unapply(path).isEmpty, "Did not expect '"+p+"' but was found by matcher")}
    }
    import Path.AccessModes.{values => modes}
    modes foreach {p =>
      val a = permissions(p)
      test(a,modes -- a)}
    modes zip util.Random.shuffle(modes) map {case (x,y) => List(x,y)} foreach {m =>
      val a = permissions(m:_*)
      test(a,modes -- a)
    }
  }
  @Test //@Ignore
  def attributeMatcher = {
    val path = fixture.path
    path.createFile()
    def test (attribute:FileAttribute[_]) = {
      path.attributes.update(attribute)
      attribute.value match {
        case time: FileTime => 
          val millis = path.attributes(attribute.name).get.asInstanceOf[FileTime].toMillis
          assertEquals(time.toMillis, millis)
        case _ => 
          assertEquals(attribute.value, path.attributes(attribute.name).get)
      }
      assertTrue(path.attributes exists attribute)
      assertTrue(PathMatcher.AttributeMatcher(attribute)(path))
    }
    
    def currentTimeSeconds = FileTime.fromMillis(FileTime.fromMillis(System.currentTimeMillis).to(TimeUnit.SECONDS) * 1000)
    test(FileAttributeImpl("lastModifiedTime",FileTime.fromMillis(1324046126000L)))
    test(FileAttributeImpl("lastModifiedTime",currentTimeSeconds))
    test(FileAttributeImpl("lastAccessTime",FileTime.fromMillis(1324046126000L)))
    test(FileAttributeImpl("lastAccessTime",currentTimeSeconds))
    assertTrue(path.attributes("size").nonEmpty)
    assertTrue(path.attributes("isRegularFile").nonEmpty)
    assertTrue(path.attributes("isDirectory").nonEmpty)
    assertTrue(path.attributes("isSymbolicLink").nonEmpty)
    assertTrue(path.attributes("isOther").nonEmpty)
    assertTrue(path.attributes("fileKey").nonEmpty)
    if (path.attributes.supportsView[DosFileAttributeView]) {
      test(FileAttributeImpl("hidden",true))
      test(FileAttributeImpl("hidden",false))
      test(FileAttributeImpl("readonly",true))
      test(FileAttributeImpl("readonly",false))
      test(FileAttributeImpl("system",true))
      test(FileAttributeImpl("archive",false))
    }
  }

  def assertMatch(exp:String)(implicit path:Path, syntax:String) = {
    assert(fixture.fs.matcher(exp,syntax)(path), "expected "+exp+" to match "+path+" when using "+syntax+" syntax")
  }
  def assertMisMatch(exp:String)(implicit path:Path, syntax:String) = {
    assert(!fixture.fs.matcher(exp,syntax)(path), "expected "+exp+" to *not* match "+path+" when using "+syntax+" syntax")
  }
  @Test //@Ignore
  def globPathMatcher = {
    implicit var path:Path = fixture.fs("a","b","c","d.x")
    implicit val syntax = PathMatcher.StandardSyntax.GLOB
    assertMatch("**/d.x")
    assertMatch("a/**/d.x")
    assertMatch("a/b/*/d.x")
    assertMatch("a/b/**")
    assertMatch("a/b/c/*")
    assertMatch("a/b/c/**")
    assertMatch("a/b/c/d.x")
    assertMatch("a/b/c/?.x")
    assertMatch("a/b/?/?.x")
    assertMatch("a/b/?/{d,e}.x")
    assertMatch("a/b/?/d?x")
    assertMatch("a/b/c/d.{x,y}")
    assertMatch("a/b/[a-z]/d.{x,y}")
    assertMatch("a/b/[!abd-z]/d.x")

    assertMisMatch("aa/**")
    assertMisMatch("A/**")
    assertMisMatch("??/**")
    assertMisMatch("a/*/d.x")
    assertMisMatch("[b-z]/**")
    assertMisMatch("[A-Z]/**")
    assertMisMatch("{b,c,d}/**")
    assertMisMatch("{A,b,c,d}/**")

    path = fixture.fs("a-","b?","c","d.x")

    assertMatch("a[-abc]/**")
    assertMatch("*/b\\?/**")
    assertMatch("**/*.*")
    assertMatch("*/**")
    assertMatch("??/**")

    assertMisMatch("a/**")
    assertMisMatch("?/**")

    // TODO RAMFS When we have a ram fs
//    path = new RamFileSystem(separator="\\")("a/","b")
//    assertMatch("""a\//b""")
//    assertMisMatch("""a//b""")
  }

  @Test //@Ignore
  def regexPathMatcher = {
    implicit var path:Path = fixture.fs("a","b","c","d.x")
    implicit val syntax = PathMatcher.StandardSyntax.REGEX
    assertMatch(".*/d.x")
    assertMatch("""(\w/)*d.x""")

    // TODO RAMFS
//    path = new RamFileSystem(separator="\\")("a/","b")
//    assertMatch("""a\//b""")
  }

  @Test //@Ignore
  def && = {
    val path = fixture.root / "a"
    path.createDirectory()

    assert((IsDirectory && NameIs("a"))(path))
    assert(!(IsDirectory && NameIs("B"))(path))
  }

  @Test //@Ignore
  def || = {
    val path = fixture.root / "a"
    path.createDirectory()

    assert((IsDirectory || NameIs("x"))(path))
    assert(!(IsFile || NameIs("x"))(path))
    assert((IsFile || NameIs("a"))(path))
  }

  @Test //@Ignore
  def - = {
    val path = fixture.root / "a"
    path.createDirectory()

    assert(!(-IsDirectory)(path))
    assert((-IsFile)(path))

    assert(!(new FunctionMatcher(-IsDirectory))(path))
    assert((new FunctionMatcher(-IsFile))(path))
  }

  @Test //@Ignore
  def -- = {
    val path = fixture.root / "a"
    path.createDirectory()

    assert((IsDirectory -- NameIs("x"))(path))
    assert(!(IsDirectory -- NameIs("a"))(path))
  }
}
