package scalax

import io.{LongTraversable, LongTraversableView}

package object io {
  type ResourceView[A] = LongTraversableView[A,LongTraversable[A]]
}
