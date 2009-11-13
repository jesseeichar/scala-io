/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

/**
 * A modularizing object for containing objects/classes related to reading lines
 */
object Line {
  object Terminators {
    /**
     * The super class for different types of line terminators/seperators
     * @see Auto
     * @see N
     * @see R
     * @see RN
     * @see Custom
     */
    sealed abstract class Terminator
    /**
     * The Auto terminator declares that the line terminator should
     * be detected.  It can detect N,R and RN line terminators
     */
    case object Auto extends Terminator
    /*
     * The \n line terminator
     */
    case object NewLine extends Terminator
    /**
     * The \r line terminator
     */
    case object CarriageReturn extends Terminator
    /**
     * The \r\n line terminator
     */
    case object Pair extends Terminator
    /**
     * A custom line terminator.  It can be an arbitrary string but
     * can be less performant than one of the other terminators
     */
    case class Custom(separator: String) extends Terminator
  }
}
