package scala.io

/**
 * Boot strap for testing framework
 */
object Tests {
  type Fixture = {def init() : Unit
                  def cleanUp() : Unit }
  val tests = ArrayBuffer[TestCase]()

  case class TestCase[F <: Fixture](name:String)() {
    protected[Tests] var test : Option[F => Unit] = None

    def must (testDec :  F => Unit) {
      if(test.isDefined) throw new IllegalStateException("Test: "+name+" already has a test declared")
      test = testDec
      Tests.tests += this
    }
  }
  
  /**
   * Run tests
   */
  def main(args:Array[String]) : Unit = {
    val results = 
      for(test <- tests) yield {
        test.test(fixture)
      }
  }
}

import Tests.{Fixture, TestCase}
trait Test[Fixture] {  
  implicit string2Test(name:String) = TestCase[F <: Fixture](name)
}
