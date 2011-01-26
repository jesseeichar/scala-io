package scalax.io

/**
 *
 *
 */
object CloseAction {
  def apply[A](f : => Unit ) = new CloseAction[A]{
    protected def closeImpl[U >: A](a:U) = f
  }
  object Noop extends CloseAction[Nothing] {
    protected def closeImpl[U >: Nothing](a:U) = ()
  }
  protected[CloseAction] class Cons[+A](before:CloseAction[A],after:CloseAction[A]) extends CloseAction[A] {
    def closeImpl[U >: A](a:U) = before(a) ++ after(a)
  }

}

/**
 * A strategy object representing
 */
trait CloseAction[+A] {
  import CloseAction.Cons

  protected def closeImpl[U >: A](a:U):Unit
  def ++[U >: A](c: CloseAction[U] ):CloseAction[U] = new Cons(this,c)
  def ::[U >: A](c: CloseAction[U] ):CloseAction[U] = new Cons(c, this)
  def apply[U >: A](u:U):List[Throwable] = {
    try {
      closeImpl(u)
      Nil
    } catch {
      case e => List(e)
    }
  }
}
