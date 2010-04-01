/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
        
    def foreach[U](f: String ⇒ U) : Unit = {
        var buffer = source.foldLeft(Buffer[Char]()) {
            case (buffer, next) if ending(buffer,next) exists terminator.lineEnd =>
//                println("buffer at end of line" + buffer + " "+next)
                if (includeTerminator) {
                    f ( (buffer mkString "") + next)
                    Buffer[Char]()
                } else {
                    if (ending(buffer,next) exists terminator.lineEnd) {
                        f(buffer.view dropRight (terminator.size - 1) mkString "")                        
                        Buffer[Char]()
                    } else {
                        // this can happen when trying to autodetect line ending
                        // the line ending may be 1 char, but it needs 2 to detect ending
                        // so the first call of #ending detects the line ending and adjusts the
                        // terminator.size to 1.  This means the last element in buffer is the 
                        // terminator and next needs to go into the buffer
                        f(buffer.view.dropRight(1) mkString "")
                        Buffer(next)
                    }

                }
            case (buffer, next) => 
//                println("buffer in line" + buffer + " "+next)
                buffer += next
                buffer
        }
        
        if (buffer.nonEmpty) {
            f (buffer mkString "")
        }
    }
}
