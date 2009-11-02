/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.util.control.ControlException

/**
 * This is a control exception that indicates the underlying filesystem object either does not exist or is not a Directory
 * <p>
 * To safely use {@link DirectoryStream} one should use the following code:
 * <pre>
 * <code>
 * import scala.util.control.Exception._
 * catching(classOf[NotADirectoryException]) opt {
 *   ds map (_.name)
 * } match {
 *   case None => println ("Oh no the path is not a directory!")
 *   case Some(names) => println ("oh everything went as planned and we got all the names: "+names)
 * }
 * </code>
 * </pre>
 */
case class NotADirectoryException() extends ControlException
