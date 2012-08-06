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
import ramfs.RamFileSystem

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
    def test (value:FileAttribute[_]) = {
      path.attributes = List(value)
      assertTrue(path.attributes exists {_ == value})
    }
    test(WriteAccessAttribute(true))
    test(WriteAccessAttribute(false))
    test(ExecuteAccessAttribute(true))
    test(ExecuteAccessAttribute(true))
    test(ReadAccessAttribute(true))
    test(ReadAccessAttribute(true))
    test(LastModifiedAttribute(1324046126000L))
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

    path = new RamFileSystem(separator="\\")("a/","b")
    assertMatch("""a\//b""")
    assertMisMatch("""a//b""")
  }

  @Test //@Ignore
  def regexPathMatcher = {
    implicit var path:Path = fixture.fs("a","b","c","d.x")
    implicit val syntax = PathMatcher.StandardSyntax.REGEX
    assertMatch(".*/d.x")
    assertMatch("""(\w/)*d.x""")

    path = new RamFileSystem(separator="\\")("a/","b")
    assertMatch("""a\//b""")
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
