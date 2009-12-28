/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import Testing._
class BasicIoTest extends Test {
  
  println("basic test being ran")
  val fixtureFactory = new FixtureFactory(){
    class BasicFileFixture extends Fixture {
      def cleanup():Unit = ()
    }
    type FixtureType = BasicFileFixture
    def apply() = new BasicFileFixture()
  }
  
  "Success" must { fixture =>
    1 + 1
  }
  
  "Failure" must { fixture =>
    assert (false, "forced failure")
  }
  
  "Error" must { fixture =>
    throw new RuntimeException("goofed up")
  }
  
  "Pending" isPending
}