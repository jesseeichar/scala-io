package scalax



/**
 * Scala IO core classes
 */
package object io {
   type OpenSeekable = Seekable {
    def position:Long
    def position_=(position:Long):Unit
  }
}
