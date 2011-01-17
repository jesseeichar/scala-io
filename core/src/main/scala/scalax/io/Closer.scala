package scalax.io

/**
 *
 * User: jeichar
 * Date: 11/19/10
 * Time: 12:00 PM
 */


object Closer {
  def apply[A](f : => Unit ) = new Closer[A]{
    protected def closeImpl[U >: A](a:U) = f
  }
  object Noop extends Closer[Nothing] {
    protected def closeImpl[U >: Nothing](a:U) = ()
  }
  protected[Closer] class Cons[+A](before:Closer[A],after:Closer[A]) extends Closer[A] {
    def closeImpl[U >: A](a:U) = before(a) ++ after(a)
  }

}


trait Closer[+A] {
  import Closer.Cons

  protected def closeImpl[U >: A](a:U):Unit
  def ++[U >: A](c: Closer[U] ):Closer[U] = new Cons(this,c)
  def ::[U >: A](c: Closer[U] ):Closer[U] = new Cons(c, this)
  def apply[U >: A](u:U):List[Throwable] = {
    try {
      closeImpl(u)
      Nil
    } catch {
      case e => List(e)
    }
  }
}
