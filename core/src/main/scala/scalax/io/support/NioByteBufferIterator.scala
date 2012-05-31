/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.support

import java.nio.ByteBuffer

class NioByteBufferIterator(var buffer: ByteBuffer) extends Iterator[Byte] {
  @inline
  final def hasNext = buffer.hasRemaining()
  @inline
  final def next = buffer.get()
}
