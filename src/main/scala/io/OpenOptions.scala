/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

trait OpenOption {}

object StandardOpenOptions {
  final val APPEND = new OpenOption{}
  final val CREATE = new OpenOption{}
  final val CREATE_NEW = new OpenOption{}
  final val DELETE_ON_CLOSE = new OpenOption{}
  final val DSYNC = new OpenOption{}
  final val READ = new OpenOption{}
  final val SPARSE = new OpenOption{}
  final val SYNC = new OpenOption{}
  final val TRUNCATE_EXISTING = new OpenOption{}
  final val WRITE = new OpenOption{}

  final val WRITE_TRUNCATE = List(CREATE, TRUNCATE_EXISTING, WRITE)
  final val WRITE_APPEND = List(CREATE, APPEND, WRITE)

  

}
