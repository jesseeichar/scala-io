/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import org.scalatest.fixture.FixtureWordSpec
import org.scalacheck.Prop
import Path.AccessModes._

trait FileSystemFixture extends FixtureWordSpec {
  case class TestData (pathString: String, access: AccessMode, numSegments:Int)
  
  abstract class Context {
    def setUp:Unit = ()
    def cleanUp:Unit = ()
    def property (test: TestData => Unit): Prop
  }

  type Fixture = Context
  
  def createContext: Context
  
  def withFixture(test: OneArgTest) {
    val context = createContext
    context.setUp
    try{
//      test(context)
    } finally {
      try {
        context.cleanUp
      } catch {
        case e => 
          System.err.println("Error while cleaning up filesystem context object")
          e.printStackTrace
      }
    }
  }
}
