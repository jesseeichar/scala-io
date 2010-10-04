/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import Matching._
import Path.AccessModes._

import org.junit.Test

abstract class FsMatchingTests extends scalax.test.sugar.AssertionSugar with Fixture {
  implicit val codec = Codec.UTF8
  
  @Test //@Ignore
  def file = {
    val path = fixture.path
    path.createFile()
    val File(f) = path // will throw MatchError is not a file
    intercept[MatchError] {val Directory(x) = path}
  }
  
  @Test //@Ignore
  def dir = {
    val path = fixture.path
    path.createDirectory()
    val Directory(d) = path // will throw MatchError is not a file
    intercept[MatchError] {val File(x) = path}
  }
  
  @Test //@Ignore
  def existance = {
    val path = fixture.path

    val NonExistent(p) = path // will throw MatchError is not a file
    intercept[MatchError] {val Exists(x) = path}
    
    path.createFile()
    
    val Exists(p2) = path // will throw MatchError is not a file
    intercept[MatchError] {val NonExistent(x) = path}
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
    modes foreach {p => test(List(p),modes - p)}
    modes zip util.Random.shuffle(modes) map {case (x,y) => List(x,y)} foreach {m => test(m,modes -- m)}
  }


  def assertMatch(exp:String)(implicit path:Path, syntax:String) = {
    assert(fixture.fs.matcher(exp,syntax)(path), "expected "+exp+" to match "+path+" when using "+syntax+" syntax")
  }
  def assertMisMatch(exp:String)(implicit path:Path, syntax:String) = {
    assert(!fixture.fs.matcher(exp,syntax)(path), "expected "+exp+" to *not* match "+path+" when using "+syntax+" syntax")
  }
  @Test //@Ignore
  def globPathMatcher = {
    implicit var path = fixture.fs("a/b/c/d.x")
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

    path = fixture.fs("a-/b?/c/d.x")

    assertMatch("a[-abc]/**")
    assertMatch("*/b\\?/**")
    assertMatch("**/*.*")
    assertMatch("*/**")
    assertMatch("??/**")

    assertMisMatch("a/**")
    assertMisMatch("?/**")
  }

  @Test //@Ignore
  def regexPathMatcher = {
    implicit var path = fixture.fs("a/b/c/d.x")
    implicit val syntax = PathMatcher.StandardSyntax.REGEX
    assertMatch(".*/d.x")
    assertMatch("""(\w/)*d.x""")
  }

}