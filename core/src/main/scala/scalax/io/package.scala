package scalax

package object io {
  type ResourceView[A] = LongTraversableView[A,LongTraversable[A]]
}
