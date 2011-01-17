/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


/*************************************************************************************
 * This file contains code samples illustrating how to use the Scala IO API.  It is  *
 * not a test file, other than it is compiled.                                       *
 *                                                                                   *
 *                                                                                   *
 * These examples will be the bases for the tests created for the IO project         *
 *                                                                                   *
 * Note: In order to be more useful all variable have the expected type explicitely  *
 *       declared so that the compiler can detect if there is a problem with the     *
 *       sample code.  It is not required to work (with the exception of the forcing *
 *       the implicits to work in the first two examples                             *
 *************************************************************************************/
object Samples {

  { // perform an actions within a file lock
    import scalax.file.{FileOps, Path}

    val file: FileOps =  Path ("file")

    implicit val codec = scalax.io.Codec.UTF8

    // By default the entire file is locked with exclusive access
    val result: Option[String] = file.withLock() { s =>
      s.slurpString
    }

    // if the filesystem does not support locking then None will be returned
    result match {
      case None => file.slurpString // oh well this is the best I can do
      case Some(data) => data
    }

    def fail: Nothing = throw new AssertionError("Uh oh")

    // or perhaps we only lock part of the file
    val result2: Traversable[Byte] = file.withLock(10, 20) { s =>
      s.bytes slice (10,20)
    } getOrElse {fail}



  }

  { // when several operation need to be performed on a file it is often more performant to perform them within an function passed to the open method
    // this is because the underlying filesystem has options for optimizing the use of the file channels
    // for example a file could be mapped into memory for the duration of the function and all operations could be performed using the same channel object
    import scalax.file.{FileOps, Path}
    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    file.open()( f => {
      val s = f.slurpString
      file.write(s.replaceAll("l", "L"))
    })
  }

}
