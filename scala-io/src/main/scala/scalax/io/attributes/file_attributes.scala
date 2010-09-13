/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
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
abstract class FileAttribute[T] (name:String, value:T)
case class GenericFileAttribute[T](name:String, value:T) extends FileAttribute[T](name,value)
case class LastModifiedAttribute(value:Long) extends FileAttribute[Long]("lastModified", value)
case class ReadAccessAttribute(readable:Boolean) extends FileAttribute[Boolean]("read", readable)
case class WriteAccessAttribute(writable:Boolean) extends FileAttribute[Boolean]("write", writable)
case class ExecuteAccessAttribute(executable:Boolean) extends FileAttribute[Boolean]("execute", executable)


