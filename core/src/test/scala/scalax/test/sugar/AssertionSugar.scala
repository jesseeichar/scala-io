/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.Writer

import scala.reflect.Manifest

import org.junit.Assert.fail
import org.junit.Assume.assumeTrue


object AssertionSugar {
  def isWindows = System.getProperty("os.name").toLowerCase.contains("win")
}

trait AssertionSugar {
  def assumeNotWindows = {
    assumeTrue(!AssertionSugar.isWindows)
  }
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

  def largeResource(key:KEY.Value):java.io.File = LargeResource.largeResource(key)
  def largeResource(key: String)(f: Writer => Unit):java.io.File = LargeResource.largeResource(key)(f)
  val Key=KEY
}

object LargeResource {
  def largeResource(key: KEY.Value):java.io.File = {
    if(key == KEY.TEXT) largeResource(key.toString)(KEY.TEXT_CREATION)
    else throw new IllegalArgumentException(key+" not recognized")
  }
  def largeResource(key: String)(f: Writer => Unit):java.io.File = {
    import scalax.io.JavaConverters._
    val tmpPath = java.io.File.createTempFile("klkjlkj","klkjlkj").getParentFile
    val file = new java.io.File(tmpPath, key)
    if(!file.exists) {
      println("Need to generate data file")
      val writer = new OutputStreamWriter(new FileOutputStream(file),"UTF-8")
      f(writer)
      writer.close
      println("Data generation complete")
    }
    file
  }
}

object KEY extends Enumeration {
  val TEXT = Value("LOTS_OF_TEXT.txt")

  val TEXT_CREATION = (writer:Writer) => {
    def lines(c:Int = 1000000) : Stream[String] = {
      util.Random.nextString(30) #:: (if(c > 0) lines(c - 1) else Stream.empty)
    }

    lines().foreach{l=>
      writer.write(l)
      writer.write("\n")
    }
    writer.close()

  }
}
