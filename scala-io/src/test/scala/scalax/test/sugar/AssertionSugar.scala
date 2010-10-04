/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar
import scala.reflect.{
  Manifest, ClassManifest
}
import ClassManifest.singleType
import org.junit.Assert._
import scalax.io.Path
import scalaio.test.Node

trait AssertionSugar {
  def ignoring[E <: Throwable](test : => Unit)(implicit m:Manifest[E]) : Unit = {
    val error = try {
      test
      Some("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e if (m >:> singleType(e)) => None
      case e => throw e;
    }
  }
  def intercept[E <: Throwable](test : => Unit)(implicit m:Manifest[E]) : Unit = {
    val error = try {
      test
      Some("Expected "+m.toString+" but instead no exception was raised")
    }catch{
      case e:AssertionError if m.erasure != classOf[AssertionError] => throw e
      case e if (m >:> singleType(e)) => None
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

  def assertSameStructure(path : Iterable[Path], tree : Seq[Node], maxDepth : Int = Int.MaxValue)
                         (implicit filter : Node => Boolean = _ => true) {
    val paths = path.toList map {_.path}
    val pathsAsString = paths mkString "\n"
    var count = 0

    def walk(tree:Seq[Node], depth : Int) : Unit =  {
      if(depth <= maxDepth ) {
        tree.filter(filter) foreach { n =>
          count += 1
          assertTrue("expected "+n.path+" to be in "+pathsAsString, paths contains {n.path})
        }
        tree foreach {n=>walk(n.children, depth+1)}
      }
    }

    walk(tree,1)

    assertEquals(count, path.size)
  }
   
}
