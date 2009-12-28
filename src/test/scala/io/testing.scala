/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io
import collection.mutable.ArrayBuffer

import java.lang.System.err
import err.{println => printErr}
import java.io.File
/**
 * Boot strap for testing framework
 */
object Testing {
  trait Fixture {
    def cleanup():Unit
  }
  
  trait FixtureFactory {
    type FixtureType <: Fixture
    def apply():FixtureType
  }
  
  case class TestCase(name : String, factory : FixtureFactory) {
    protected[Testing] var test : Option[factory.FixtureType => Unit] = None

    def isPending = {
      if(!tests.contains(this)) tests += this
    }

    def must (testDec :  factory.FixtureType => Unit) {
      if(test.isDefined) throw new IllegalStateException("Test: "+name+" already has a test declared")
      test = Some(testDec)
      tests += this
    }
  }

  sealed abstract class TestResult
  case class TestPassed(name:String) extends TestResult
  case class TestSkipped(name:String) extends TestResult
  case class TestFailed(name:String, failure : Throwable) extends TestResult
  case class TestError(name:String, failure : Throwable) extends TestResult
  
  private val tests = ArrayBuffer[TestCase]()
  
  final val STACK_TRACE = "trace"
  
  /**
   * Run tests
   */
  def main(args:Array[String]) : Unit = {
    val params = Map(args partialMap {case STACK_TRACE => STACK_TRACE -> true} :_*)
    
    loadTests()
    
    println("Let the testing commence...")
    
    val results = 
      for(test <- tests) yield {
        val fixture = test.factory()
        try {
          test.test match {
            case Some(t) => 
              t(fixture) 
              TestPassed(test.name)
            case None => 
              TestSkipped(test.name)
          }
        } catch {
          case e:AssertionError => TestFailed(test.name, e)
          case e => TestError(test.name, e)
        } finally {
          fixture.cleanup
        }
     }
     
    results foreach {
      case TestPassed(name) =>
        println("   "+name)
      case TestSkipped(name) =>
        println("s  "+name)
      case TestFailed(name, e) if(params.getOrElse(STACK_TRACE, false)) => 
        println("x  "+name+": "+e.getMessage)
        e.printStackTrace
      case TestFailed(name, e) => 
        println("x  "+name+": "+e.getMessage)
      case TestError(name, e) if(params.getOrElse(STACK_TRACE, false)) => 
        println(" X "+name+": "+e.getMessage)
        e.printStackTrace
      case TestError(name, e) => 
        println(" X "+name+": "+e.getMessage)
    }
    val (passed, failed, errors) =  ((0,0,0) /: results) {
      case ((passed, failed, errors), TestPassed(_)) => (passed + 1, failed, errors)
      case ((passed, failed, errors), TestFailed(_,_)) => (passed, failed + 1, errors)
      case ((passed, failed, errors), TestError(_,_)) => (passed, failed, errors + 1)
      case (r, TestSkipped(_)) => r
    }
    
    val skipped = results.size - errors - passed - failed
    println("")
    printf("Total: %s, Passed: %s, Failed: %s, Errors: %s, Skipped: %s\n",results.size, passed, failed, errors, skipped)
  }
  
  private def search(f:File):Seq[File] = {
    if(f.isFile) List(f).filter(_.getName endsWith ".class")
    else {
      val all = Option(f.listFiles) getOrElse {Array[File]()}
      val filtered = all filter {file => file.isDirectory || file.getName.endsWith(".class")}
      filtered flatMap {search _}
    }
  }
  
  private def loadTests() : Traversable[Either[Class[_], Test]] = {
    val loader = getClass.getClassLoader
    
    // for now I am assuming test are not packaged in a jar
    val baseDir = new File(loader.getResource(".").getFile)
    
    
    val potentialTestClasses =
      for {file <- search(baseDir)
           if(file.getName.contains("Test"))
          } yield {
            val relativePath = file.getPath.drop(baseDir.getPath.size+1)
            val className = relativePath.replace(File.separator, ".").dropRight(".class".size)
            loader.loadClass(className)
          }
    
    for {c <- potentialTestClasses
         if (classOf[Test].isAssignableFrom(c) && c != classOf[Test]) 
        } yield { 
          println("instantiating "+ c)
          
          try   {Right(c.newInstance.asInstanceOf[Test])} 
          catch {case _:InstantiationException => Left(c)}
        }
  }
}

import Testing.{FixtureFactory, TestCase}

trait Test {
  val fixtureFactory : FixtureFactory
  implicit def string2Test(name:String) = TestCase(name, fixtureFactory)
}
