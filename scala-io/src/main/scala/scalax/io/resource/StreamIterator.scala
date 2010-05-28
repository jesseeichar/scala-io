/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.Codec
import java.io.{
    InputStream, Reader
}

protected[resource] class StreamIterator[E](nextElem : => Option[E]) extends Iterator[E] {
    var i = nextElem
    def hasNext = i.isDefined
    def next() : E = {
        if(!hasNext) throw new java.util.NoSuchElementException()
        
        val n = i
        i = nextElem
        n.get
    }
}

object StreamIterator {
    def apply[E](nextElem : => Option[E]) = new StreamIterator(nextElem)
    def apply(inputStream : InputStream) = apply[Int] {
        val i = inputStream.read()
        if(i == -1) None
        else Some(i)
    }
    def apply(reader : Reader) = apply[Char] {
        val i = reader.read()
        if(i == -1) None
        else Some(i.toChar)
    }
    def apply(reader : Reader, from : Codec, to : Codec ) = apply[Char] {
        val i = reader.read()
        if(i == -1) None
        else Some(from.translate(i.toChar.toString)(to).head)
    }
}