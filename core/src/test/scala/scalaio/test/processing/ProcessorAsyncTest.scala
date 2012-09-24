package scalaio.test.processing

import scalax.test.sugar.AssertionSugar
import org.junit.Test
import org.junit.Assert._
import scalax.io._
import processing._
import java.util.concurrent.TimeoutException
import scala.concurrent.util.duration._
import scala.util.{Success, Failure}

class ProcessorAsyncTest extends AssertionSugar{

  val factory = new ProcessorFactory(DefaultResourceContext)
  @Test
  def processor_async_must_timeout {

    // repeat test many times to verify that there are no Heisenbugs
    for(i <- 1 to 20) {
      val p = factory{Thread.sleep(500); Some(1)} timeout 10.milliseconds
      intercept[TimeoutException] {
        p.acquireAndGet(v => v)
      }

      val p2 = factory{Thread.sleep(10); Some(1)} timeout 30.seconds
      assertEquals(Some(1), p2.acquireAndGet(v => v))

      var success = false

      intercept[TimeoutException] {
        for {v <- p} success = true
      }
      assertFalse(success)

      for {v <- p2} success = true
      assertTrue(success)
    }
  }

  @Test(timeout=1000L)
  def processor_future_should_complete {

    var success = false
    val future = (factory{Thread.sleep(10); Some(1)}).future

    implicit val executionContext = scalax.io.executionContext
    
    future.onComplete{
      case Failure(e) => success = false
      case Success(r) => success = true
    }
    
    while(!future.isCompleted) {
      Thread.sleep(30);
    }

    assertTrue(success)
  }
}
