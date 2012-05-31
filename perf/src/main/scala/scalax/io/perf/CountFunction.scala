package scalax.io.perf

class CountFunction[@specialized(Char,Byte) A] extends Function[A,Unit] {
    private[this] var i = 0
    override def apply(b: A) { i += 1 }
}
