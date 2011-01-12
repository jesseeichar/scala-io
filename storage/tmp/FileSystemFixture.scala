/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scala.collection.mutable.ListBuffer
import org.scalatest.fixture.FixtureWordSpec
import org.scalacheck.{
  Gen, Arbitrary, Prop
}
import Arbitrary._
import Path.AccessModes._

trait FileSystemFixture extends FixtureWordSpec {
  implicit val codec = Codec.UTF8
  case class TestData (pathString: String, numSegments:Int, access: AccessMode*)

  abstract class Context {
    private val all = ListBuffer[Path]()
    protected val defaultGen : Generator = {
      val fileNameGen = for {
       numSegments <- Gen.choose(1,10)
       filename <- genName(numSegments)
       access <-
      }
      null
    }
    def setUp : Unit = ()
    def cleanUp : Unit = all foreach {_.deleteRecursively()}
    def generator : Gen[TestData]
    def property (test : TestData => Unit): Prop = {
      Prop.forAll(generator){test}
    }
    def testData : TestData = {
      val data = createData
      all += Path(data.pathString)
      data
    }
    protected def createData : TestData
    def path : Path = Path(testData.pathString)
    def file : Path = {
      val path = Path(testData.pathString)
      path.fileOps.writeString("Testdata")
      path
    }
    def dir : Path = path.createDirectory()
    def tree : Path = {
      val base = dir
      val bf = (base / "bf").fileOps writeString ("bf")
      val bdf = (base / "bd" / "bdf").createFile().fileOps writeString ("bdf")

      base
    }
  }

  type FixtureParam = Context

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
