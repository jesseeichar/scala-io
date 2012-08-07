package scalax.file
import scala.collection.generic.CanBuildFrom
import scalax.io.AbstractLazyIteratorBasedBuilder
import scala.collection.TraversableLike
import scalax.io.CloseableIterator
import scalax.io.CloseableIteratorOps

trait PathSetLike[+T, +Repr <: PathSetLike[T, Repr]] extends PathFinder[T] with TraversableLike[T, Repr] {
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
    bf.apply(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(() => CloseableIteratorOps(iterator).map(f))
        ci.result()
      case b =>
        withIterator(it => b ++= it.map(f))
        b.result()
    }
  }
  override /*TraversableLike*/ def flatMap[B, That](f: T => collection.GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    bf.apply(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(() => CloseableIteratorOps(iterator).flatMap(f))
        ci.result()
      case b =>
        withIterator(it => b ++= it.flatMap(f))
        b.result()
    }
  }
  override def filter(f: T => Boolean) = {
    newBuilder match {
      case ci: AbstractLazyIteratorBasedBuilder[T, Repr] =>
        ci.addIter(() => CloseableIteratorOps(iterator).filter(f))
        ci.result()
      case b =>
        withIterator(it => b ++= it.filter(f)) 
        b.result()
    }
  }
  override def collect [B, That] (pf: PartialFunction[T, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    bf(repr) match {
      case ci: AbstractLazyIteratorBasedBuilder[B, That] =>
        ci.addIter(() => CloseableIteratorOps(iterator).collect(pf))
        ci.result()
      case b =>
        withIterator(it => b ++= it.collect(pf))
        b.result()
    }
  }
  
  override def foreach[U] (f: T => U): Unit = withIterator{i => while (i.hasNext) f(i.next)}
  override def head = withIterator{_.next}
  override def headOption = withIterator{i => if(i.hasNext) Some(i.next) else None}
  override /*TraversableLike*/ def drop(n: Int): Repr = slice(n,Int.MaxValue)
  override /*TraversableLike*/ def dropWhile(p: T => Boolean): Repr = build(_.dropWhile(p))
  override /*TraversableLike*/ def takeWhile(p: T => Boolean): Repr = build(_.takeWhile(p))
  override /*TraversableLike*/ def take(n: Int): Repr = slice(0, n)
  override /*TraversableLike*/ def slice(from:Int, until:Int) = build(_.lslice(from,until))
  override /*TraversableLike*/ def forall(p: T => Boolean): Boolean =
    withIterator(_ forall p)
  override /*TraversableLike*/ def exists(p: T => Boolean): Boolean =
    withIterator(_ exists p)
  override /*TraversableLike*/ def find(p: T => Boolean): Option[T] =
    withIterator(_ find p)
  override /*TraversableLike*/ def isEmpty: Boolean =
    !withIterator(_.hasNext)

  override /*TraversableLike*/ def foldRight[B](z: B)(op: (T, B) => B): B =
    withIterator(_.foldRight(z)(op))
  override /*TraversableLike*/ def reduceRight[B >: T](op: (T, B) => B): B =
    withIterator(_.reduceRight(op))

  private def build[B >: T](f: CloseableIteratorOps[T] => CloseableIterator[B]): Repr = newBuilder match {
      case ci: AbstractLazyIteratorBasedBuilder[B, Repr] =>
        ci.addIter(() => f(CloseableIteratorOps(iterator)))
        ci.result()
      case b =>
        withIterator(it => b ++= f(CloseableIteratorOps(it)).asInstanceOf[CloseableIterator[T]]) 
        b.result()
    }

  protected def withIterator[R](f: CloseableIterator[T] => R) = {
    val i = iterator
    try f(i)
    finally i.close
  } 
  
  def iterator: CloseableIterator[T]
}
