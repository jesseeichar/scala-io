package scalax

import scala.concurrent.ExecutionContext

/**
 * Scala IO core classes
 */
package object io {
  lazy val executionContext = ExecutionContext.global
  type SeekableByteChannel = java.nio.channels.SeekableByteChannel
  type OpenOption = java.nio.file.OpenOption
}
