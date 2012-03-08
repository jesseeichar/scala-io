/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/**
 * Examples using file locks and executing multiple operations
 * within the context of a single open file
 */
object FileLockingAndBlockExecution {

  /**
   * Perform an actions within a file lock
   */
  def lock{
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

  /**
   * when several operation need to be performed on a file it is often more performant
   * to execute them within an function passed to the open method
   * this is because the underlying filesystem has options for optimizing
   * the use of the file channels for example a file could be mapped into memory
   * for the duration of the function and all operations could be performed using the
   * same channel object
   */
  def operationBlock {
    import scalax.file.{FileOps, Path}
    // see codec examples in scala io core for details on why there is an implicit codec here
    implicit val codec = scalax.io.Codec.UTF8

    val file: FileOps =  Path ("file")

    // There are several patterns of usage for processors
    // this demonstrates one.
    for {
      // create a processor
      processor <- file.seekableProcessor()
      // read in the full text of the file
      string <- processor.slurpString()
      // capitize all letter L
      _ <- processor.write(string.replaceAll("l", "L"))
    } ()  
    // the body of for is empty in this example but does not have to be.
    // if the for comprehension contains a yield the processor will not execute
    // until the processor that results from the for-comprehension is executed
    
  }

}
