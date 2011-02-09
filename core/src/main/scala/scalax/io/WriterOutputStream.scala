/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{
    OutputStream, Writer
}

/**
 * Takes a writer and allows it to be treated like a OutputStream,  The data is encoded as it is written to the Writer
 *
 * '''Not API''' just a support class for implementation
 */
protected[io] class WriterOutputStream(writer : Writer)( implicit codec : Codec) extends OutputStream {
    private val encoding = codec.name

    override def write(b : Array[Byte]) = {
        writer.write(new String(b,encoding));
    }

    override def write(b : Array[Byte], off : Int, len : Int) = {
        writer.write(new String(b,off,len,encoding));
    }

    def write(b : Int) = {
        write(Array(b.toByte));
    }

    override def flush() : Unit = {
        writer.flush
        super.flush
    }

    override def close() : Unit = {
        writer.close()
        super.close()
    }
}
