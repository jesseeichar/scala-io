/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.Codec
import java.io._

protected[resource] class CharInputStream(source : Either[Reader,Iterator[Char]])( implicit codec : Codec) extends InputStream {
    val iter = source match {
        case Left(reader) => StreamIterator(reader)
        case Right(iterator) => iterator
    }
    
    var bytes : Array[Byte] = new Array[Byte](0)
    var i = 0
    
    val encodeBuffer = Array[Char](1)
    def read : Int = {
       val h = if(i == bytes.size) {
            if(iter.hasNext) {
                System.out.println("1")
                i = 1
                encodeBuffer(0) = iter.next
                bytes = codec encode encodeBuffer
                bytes(0)
            } else {
                System.out.println("2")
                -1
            }            
        } else {
            System.out.println("3")
            i += 1
            bytes(i-1)
        }

            
        System.out.println(h)
        h
    }
    
    override def close() = source match {
        case Left(reader) => reader.close()
        case _ => ()
    }
    
}
