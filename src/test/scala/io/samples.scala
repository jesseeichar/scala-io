/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
 *************************************************************************************/

object Samples {
  { // implicitly convert strings to paths
    import scalax.io.Path
    import Path.string2path
    
    val filePath:Path = "/tmp/file"
  }
  
  { // implicitly convert files to paths
    import java.io.File
    import scalax.io.Path
    import Path.string2path
    
    val filePath:Path = new File("/tmp/file")
  }

    
}
