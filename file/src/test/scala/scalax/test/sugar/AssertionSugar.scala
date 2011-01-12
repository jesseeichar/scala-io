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
import org.junit.Assert._
import scalax.file.Path
import scalaio.test.Node

trait Assert extends AssertionSugar{

  def assertSameStructure(path : Iterable[Path], tree : Seq[Node], maxDepth : Int = Int.MaxValue)
                         (implicit filter : Node => Boolean = _ => true) {
    val pathList = path.toList
    val sep = path.headOption.map { _.separator} getOrElse Node.Sep
    val pathsAsString = pathList map {_.path} mkString "\n"
    var count = 0

    def contains(desiredPathName:String) = {
      pathList exists {p =>
        desiredPathName == p.path
      }
    }
    def walk(tree:Seq[Node], depth : Int) : Unit =  {
      if(depth <= maxDepth ) {
        tree.filter(filter) foreach { n =>
          count += 1
          val nodePath = n.path replace (Node.Sep, sep) replace (sep+sep,sep)
          assertTrue("expected "+nodePath+" to be in "+pathsAsString, contains(nodePath))
        }
        tree foreach {n=>walk(n.children, depth+1)}
      }
    }

    walk(tree,1)

    assertEquals(count, path.size)
  }

}
