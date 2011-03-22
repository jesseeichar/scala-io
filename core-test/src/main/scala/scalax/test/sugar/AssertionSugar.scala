/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar
import scala.reflect.Manifest
import org.junit.Assert._
import scalax.io.Output



trait AssertionSugar {
  def ignoring[E <: Throwable](test : => Unit)(implicit m:Manifest[E]) : Unit = {
    val error = try {
      test
      Some("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e if (m >:> Manifest.singleType(e)) => None
      case e => throw e;
    }
  }
  def intercept[E <: Throwable](test : => Unit)(implicit m:Manifest[E]) : Unit = {
    val error = try {
      test
      Some("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e:AssertionError if m.erasure != classOf[AssertionError] => throw e
      case e if (m >:> Manifest.singleType(e)) => None
      case e =>
        e.printStackTrace
        Some("Expected "+m.toString+" but instead got "+e.getClass)
    }

    error match {
      case Some(msg) => fail(msg)
      case None => ()
    }
  }

  def repeat[U] (f : => U)(implicit times : Int = 50) = 1 to times foreach {_ => f}

  def largeResource(key:KEY.Value):java.io.File = {
    if(key == KEY.TEXT) largeResource(key, KEY.TEXT_CREATION)
    else throw new IllegalArgumentException(key+" not recognized")
  }
  def largeResource(key:KEY.Value, f : Output => Unit):java.io.File = {
    import Output._
    
    val tmpPath = java.io.File.createTempFile("klkjlkj","klkjlkj").getParentFile
    val file = new java.io.File(tmpPath, key.toString)
    if(!file.exists) {
      println("Need to generate data file")
      f(file.asOutput)
      println("Data generation complete")
    }
    file
  }
  val Key=KEY
}

object KEY extends Enumeration {
  val TEXT = Value("LOTS_OF_TEXT.txt")
  
  val TEXT_CREATION = (out:Output) => {
    def lines(c:Int = 1000000) : Stream[String] = {
      util.Random.nextString(30) #:: (if(c > 0) lines(c - 1) else Stream.empty)
    }
    out.writeStrings(lines(),"\n")      
  }
}
