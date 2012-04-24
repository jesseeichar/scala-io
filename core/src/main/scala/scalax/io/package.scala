package scalax

import scala.concurrent.ExecutionContext

/**
 * Scala IO core classes
 */
package object io {
  protected[io] lazy val executionContext = ExecutionContext.defaultExecutionContext
}
