/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.test.sugar

import java.io._
import scalax.io.Codec

trait IOSugar {
    implicit def stringToStringExtras(s:String) = new {
        def inputStream(implicit codec : Codec = Codec.default) = new ByteArrayInputStream(s getBytes codec.name)
    }
}
