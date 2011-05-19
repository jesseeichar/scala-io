/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io


class ReaderResourceTraversableViewTest extends ResourceTraversableViewTest {

  protected override def expectedData(tsize:Int,
                             dataFunc: (Int) => Seq[Int]) =
    dataFunc(tsize) mkString "" map {_.toInt}

  override def traversable[U, A](tsize: Int,
                                 callback: (Int) => U,
                                 dataFunc: (Int) => Traversable[Int],
                                 conv: (Int) => A) = {
    val callBackAndConv = (c:Char) => {
      val i = c.toInt
      callback(i)
      conv(i)
    }
    val data = dataFunc(tsize) mkString ""
    def resource = Resource.fromReader(new java.io.StringReader(data))
    ResourceTraversable.readerBased(resource.open,initialConv = callBackAndConv).view
  }

}
