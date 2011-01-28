package scalax

import io.{LongTraversable, LongTraversableView}

/**
 * Scala IO core classes
 */
package object io {
  type ResourceView[A] = LongTraversableView[A,LongTraversable[A]]
}
