/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.attributes

/**
 * Represents a implementation specific attribute
 * of a file or directory.
 * 
 * @author  Jesse Eichar
 * @since   1.0 
 */
case class FileAttribute[T] (name:String, value:T)
