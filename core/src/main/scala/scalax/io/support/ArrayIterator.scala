/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.support

class ArrayIterator[A](var a:Array[A],var end:Int) extends Iterator[A]{
  var start=0
  var now=0
  @inline
  final def hasNext = now < end
  @inline
  final def next = {
    now += 1
    a(now - 1)
  }
}
