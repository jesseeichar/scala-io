/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.util.control.ControlException
import java.io.IOException

case class NoSuchFileException() extends IOException with ControlException

/**
 * This is a control exception that indicates the underlying filesystem object cannot be treated as a File.
 * <p>
 * IE a symbolic link maybe treated as a file in some cases but a Directory cannot.  So
 * if a file operation is attempted on a Directory a NotFileException will be thrown
 * <p>
 * To safely use {@link File} one should use the following code:
 * <pre>
 * <code>
 * import scala.util.control.Exception._
 * catching(classOf[NotFileException]) opt {
 *   file.lines foreach (println _)
 * } match {
 *   case None => println ("Oh no the path is not a file")
 *   case Some(names) => println ("oh everything went as planned and we got all the lines: "+lines)
 * }
 * </code>
 * </pre>
 */
case class NotFileException() extends IOException with ControlException

/**
 * This is a control exception that indicates the underlying filesystem object either does not exist or is not a Directory
 * <p>
 * To safely use {@link DirectoryStream} one should use the following code:
 * <pre>
 * <code>
 * import scala.util.control.Exception._
 * catching(classOf[NotDirectoryException]) opt {
 *   ds map (_.name)
 * } match {
 *   case None => println ("Oh no the path is not a directory!")
 *   case Some(names) => println ("oh everything went as planned and we got all the names: "+names)
 * }
 * </code>
 * </pre>
 */
case class NotDirectoryException() extends IOException with ControlException
