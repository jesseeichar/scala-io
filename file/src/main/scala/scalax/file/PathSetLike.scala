package scalax.file
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scalax.io.AbstractLazyIteratorBasedBuilder

trait PathSetLike[+T, +Repr <: PathSetLike[T, Repr]] extends IterableLike[T, Repr] with PathFinder[T] {
  self =>

  override protected[this] def thisCollection: PathSet[T] = this.asInstanceOf[PathSet[T]]
  override protected[this] def toCollection(repr: Repr): PathSet[T] = repr.asInstanceOf[PathSet[T]]

  /**
   * Load the set into memory and a more standard set
   */
  def force : Iterable[T] = Iterable.empty ++ this

  /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.*/
  def +++[U >: T](includes: PathFinder[U]): PathSet[U]

  /**Excludes all paths from <code>excludes</code> from the paths selected by this <code>PathSet</code>.*/
  def ---[U >: T](excludes: PathFinder[U]): PathSet[T]

  /**
   * Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * descendants of paths selected by this finder.
   */
  def **[F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[T]

  def *** : PathSet[T]

  /**
   * Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * immediate children of paths selected by this finder.
   */
  def *[F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[T]

  /**
   * Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def /(literal: String): PathSet[T]

  /**
   * Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def \(literal: String) = /(literal)

  override /*TraversableLike*/ def map[B, That](f: T => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val it = () => iterator.map(f)
    bf.apply(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(it)
        ci.result()
      case b =>
        b ++= it()
        b.result()
    }
  }
  override /*TraversableLike*/ def flatMap[B, That](f: T => collection.GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val it = () => iterator.flatMap(f)
    bf.apply(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(it)
        ci.result()
      case b =>
        b ++= it()
        b.result()
    }
  }
  override def filter(f: T => Boolean) = {
    val it = () => iterator.filter(f)
    newBuilder match {
      case ci: AbstractLazyIteratorBasedBuilder[T, Repr] =>
        ci.addIter(it)
        ci.result()
      case b =>
        b ++= it()
        b.result()
    }
  }
  override def collect [B, That] (pf: PartialFunction[T, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val it = () => iterator.collect(pf)
    bf(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(it)
        ci.result()
      case b =>
        b ++= it()
        b.result()
    }
  }
}
