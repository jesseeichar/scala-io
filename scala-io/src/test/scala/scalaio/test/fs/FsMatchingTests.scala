/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalax.io.Path.Matching._

import org.junit.Assert._
import org.junit.{
  Test,Ignore
}

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
}