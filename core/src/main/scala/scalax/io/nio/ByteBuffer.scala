/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.nio

import scalax.io.Codec
import java.nio.{ByteBuffer => NioByteBuffer}

/**
 * Allows use of a java.nio.ByteBuffer as an IndexedSeq.  But remember that the underlying buffer
 * is mutable so this object can be mutated by the underlying object
 */
class ByteBuffer(buf : NioByteBuffer) extends IndexedSeq[Byte] {
  def apply(idx : Int) = {buf.get(idx)}
  def length = buf.limit
}
