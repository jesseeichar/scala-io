/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.mutable.Buffer
import Line.Terminators._

class LineTraverseable(source: Traversable[Char], terminator: Terminator, includeTerminator: Boolean) extends Traversable[String] {
    
    private def ending(line : Seq[Char], next : Char) = {
        if(line.size < terminator.size - 1){
            None
        } else {
            Some(line.takeRight(terminator.size - 1) :+ next)
        }
    }

    def foreach[U](f: String => U) : Unit = {
        var buffer = source.foldLeft(Buffer[Char]()) {
            case (buffer, next) if ending(buffer,next) exists terminator.lineEnd =>
                // ending must be checked because when trying to autodetect line ending
                // the line ending may be 1 char, but it needs 2 to detect ending
                // so the first call of #ending detects the line ending and adjusts the
                // terminator.size to 1.  This means the last element in buffer is the 
                // terminator and next needs to go into the buffer
                ( includeTerminator, ending(buffer,next) exists terminator.lineEnd) match {
                    case (true, true) =>
                        f ( (buffer mkString "") + next)
                        Buffer[Char]()
                    case (true,false) => 
                        f (buffer mkString "")
                        Buffer[Char](next)
                    case (false,true) => 
                        f(buffer.view dropRight (terminator.size - 1) mkString "")                        
                        Buffer[Char]()
                    case (false,false) =>
                        f(buffer.view.dropRight(1) mkString "")
                        Buffer(next)
                }
            case (buffer, next) => 
                buffer += next
                buffer
        }

        if (buffer.nonEmpty) {
            f (buffer mkString "")
        }
    }
}
