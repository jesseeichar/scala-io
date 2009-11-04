/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

trait OpenOptions {}

object StandardOpenOptions {
  final val APPEND = new OpenOptions{}
  final val CREATE = new OpenOptions{}
  final val CREATE_NEW = new OpenOptions{}
  final val DELETE_ON_CLOSE = new OpenOptions{}
  final val DSYNC = new OpenOptions{}
  final val READ = new OpenOptions{}
  final val SPARSE = new OpenOptions{}
  final val SYNC = new OpenOptions{}
  final val TRUNCATE_EXISTING = new OpenOptions{}
  final val WRITE = new OpenOptions{}

  final val WRITE_TRUNCATE = List(CREATE, TRUNCATE_EXISTING, WRITE)
  final val WRITE_APPEND = List(CREATE, APPEND, WRITE)

  

}
