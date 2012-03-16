package scalax

import akka.dispatch.ExecutionContext
import akka.actor.ActorSystem


/**
 * Scala IO core classes
 */
package object io {
  protected[io] implicit val executionContext = ExecutionContext.defaultExecutionContext(ActorSystem.create())
}
