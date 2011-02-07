package scalax

import io.{LongTraversable, LongTraversableView}

/**
 * Scala IO core classes
 */
package object io {
  /**
   * An alias from LongTraversableView[A,LongTraversable[A]]. The only purpose
   * is to make the type signatures easier to read
   */
  type ResourceView[A] = LongTraversableView[A,LongTraversable[A]]
}
