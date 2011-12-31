trait I[E]
case class El[E](e:E) extends I[E]
case object EOF extends I[Nothing]

trait Consumer[E,A] {
	def run:A = null.asInstanceOf[A]
}
case class Done[E,A](a:A, e:I[E]) extends Consumer[E,A]
case class Cont[E,A](f:I[E] => Consumer[E,A]) extends Consumer[E,A]

