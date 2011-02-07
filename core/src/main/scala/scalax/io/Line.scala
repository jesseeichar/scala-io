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
    protected[io] case class LineSplit(line:Seq[Char], term:Seq[Char] = "", nextLine:Seq[Char] = "") {
      def toString(includeTerminator:Boolean) = if(includeTerminator) line ++ term mkString "" else line mkString ""
    }

    /**
     * The super class for different types of line terminators/separators
     *
     * @see [[scalax.io.LineTraversable]]
     */
    sealed abstract class Terminator {
      /**
       * Splits the characters into three parts, the line, the terminator string and the nextLine.
       */
      def split(chars: Seq[Char]) : LineSplit
    }

    /**
     * The Auto terminator detects the line terminator.
     * It can detect N,R and RN line terminators
     */
    case class Auto() extends Terminator {
        private var choices = RNPair :: NewLine :: CarriageReturn ::  Nil

        def split(chars: Seq[Char]) = synchronized {
          val splits = choices.view.map{c => (c,c split chars)}
          //val pairDoesNotMatch = splits.get(RNPair).forall{_.term isEmpty}
          splits.find{_._2.term.nonEmpty} match {
            case None =>
              splits.head._2
            case Some((RNPair,split)) =>
              choices = RNPair :: Nil
              split
            case Some((choice,split)) =>
              if(split.nextLine.nonEmpty)
                choices = choice :: Nil
              split

          }
        }
    }
    private[Terminators] abstract class Simple(val sep:String) extends Terminator {
        private val custom = Custom(sep)
        def split(chars: Seq[Char]) = custom.split (chars)
    }
    /*
     * The \n line terminator
     */
     case object NewLine extends Simple("\n")
    /**
     * The \r line terminator
     */
    case object CarriageReturn extends Simple("\r")
    /**
     * The \r\n line terminator
     */
    case object RNPair extends Simple("\r\n")
    /**
     * A custom line terminator.  It can be an arbitrary string but
     * can be less performant than one of the other terminators
     */
    case class Custom(separator: String) extends Terminator {
        def split(chars: Seq[Char]) = {
            chars.indexOfSlice(separator) match {
              case -1 => LineSplit(chars)
              case i =>
                val (line, rest) = chars.splitAt(i)
                val nextLine = rest.drop(separator.length)
                LineSplit(line,separator,nextLine)
            }
        }
    }
  }
}
