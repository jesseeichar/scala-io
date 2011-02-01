/*                      __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scalax.io

/**
 * Factory for [[scalax.io.CloseAction]].  Also contains the [[scalax.io.CloseAction.Noop]] CloseAction
 */
object CloseAction {
  /**
   * Create a [[scalax.io.CloseAction]] from a function.  The function is the action that is performed
   * on close.
   */
  def apply[A](f : A => Unit ) = new CloseAction[A]{
    protected def closeImpl(a:A) = f(a)
  }

  /**
   * The unit/nothing [[scalax.io.CloseAction]]
   */
  object Noop extends CloseAction[Any] {
    protected def closeImpl(a:Any) = ()
    override def +:[B <: Any](o:CloseAction[B]):CloseAction[B] = o
    override def :+[B <: Any](o:CloseAction[B]):CloseAction[B] = o
  }
  protected[CloseAction] class Prepend[A,B >: A](a:CloseAction[A],b:CloseAction[B]) extends CloseAction[A] {
    protected def closeImpl(p:A):Unit = ()
    override def apply(u:A):List[Throwable] = a(u) ++ b(u)
  }
  protected[CloseAction] class Append[A,B >: A](a:CloseAction[A],b:CloseAction[B]) extends CloseAction[A] {
    protected def closeImpl(p:A):Unit = ()
    override def apply(u:A):List[Throwable] = b(u) ++ a(u)
  }
}

/**
 * A strategy object representing an action to take upon closing a [[scalax.io.Resource]].
 * CloseActions can be composed using the +: and :+ method and can be added to a [[scalax.io.Resource]]
 * when it is created or added to an existing [[scalax.io.Resource]] object.
 *
 * If an exception is raised by the CloseAction it will be cause by the apply method and will be
 * returned to the Resource calling the exception.   Normally the exception will be added to the collection
 * of Throwables in the Resource so errors triggered by the CloseActions will be obtainable from the Resource.
 *
 * Also related to exceptions is the fact that all CloseActions will be executed when they are combined.
 * For example if CloseAction A and B are combined and A fails with an exception B will still be executed.
 *
 * The easiest method for creating a close action is to simply call
 * the [[scalax.io.CloseAction]].apply factory method.  It takes a function and contructs
 * a CloseAction from it.
 *
 * {{{
 * val loggingCloseAction = CloseAction[Any]{_ =>
 *    println("InputStream "+r+" is closing")
 * }
 * }}}
 *
 * @tparam A The type of object this action
 */
trait CloseAction[-A] {
  import CloseAction.{Prepend,Append,Noop}

  /**
   * Construct a new CloseAction consisting of ''other'' then ''this''.
   *
   * @param other an action to ''prepend'' to this action
   * @return an action consisting of first executing ''other'' then ''this''
   */
	def +:[B <: A](other:CloseAction[B]):CloseAction[B] = other match {
    case Noop => this
    case _ => new Prepend(other,this)
  }
  /**
   * Construct a new CloseAction consisting of ''this'' then ''other''.
   *
   * @param other an action to ''append'' to this action
   * @return an action consisting of first executing ''this'' then ''other''
   */
	def :+[B <: A](other:CloseAction[B]):CloseAction[B] = other match {
    case Noop => this
    case _ => new Append(other,this)
  }

  /**
   * The actual implementation of the action.  Implementers of a CloseAction
   * must implement this method.
   *
   * @param resource the resource that will be closed.
   */
  protected def closeImpl(resource:A):Unit

  /**
   * Execute the action and return any Exceptions that may have
   * been raised during the execution of this method.
   *
   * @param resource the resource being closed
   * @return the errors that occurred while executing the message.
   */
  def apply(resource:A):List[Throwable] = {
    try {
      closeImpl(resource)
      Nil
    } catch {
      case e => List(e)
    }
  }
}
