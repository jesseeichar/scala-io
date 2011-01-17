package scalax.io

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import java.io._

protected[io] class CharInputStream(source : Either[Reader,Iterator[Char]])( implicit codec : Codec) extends InputStream {
    val iter = source match {
        case Left(reader) => StreamIterator(reader)
        case Right(iterator) => iterator
    }

    var bytes : Array[Byte] = new Array[Byte](0)
    var i = 0

    val encodeBuffer = Array[Char](1)
    def read : Int = {
       if(i == bytes.size) {
            if(iter.hasNext) {
                i = 1
                encodeBuffer(0) = iter.next
                bytes = codec encode encodeBuffer
                bytes(0)
            } else {
                -1
            }
        } else {
            i += 1
            bytes(i-1)
        }
    }

    override def close() = source match {
        case Left(reader) => reader.close()
        case _ => ()
    }

}
