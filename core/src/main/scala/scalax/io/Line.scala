/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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
     * The super class for different types of line terminators/separators
     *
     * @see [[scalax.io.LineTraversable]]
     */
    sealed abstract class Terminator
    /**
     * The Auto terminator detects the line terminator.
     * It can detect N,R and RN line terminators
     */
    case object Auto extends Terminator

    abstract class SimpleTerminator(val sep:String) extends Terminator
    /*
     * The \n line terminator
     */
     case object NewLine extends SimpleTerminator("\n")
    /**
     * The \r line terminator
     */
    case object CarriageReturn extends SimpleTerminator("\r")
    /**
     * The \r\n line terminator
     */
    case object RNPair extends SimpleTerminator("\r\n")
    /**
     * A custom line terminator.  It can be an arbitrary string
     */
    case class Custom(separator: String) extends SimpleTerminator(separator)
  }
}
