package scalaio.test

import scalax.io._
import Input._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
Test, Before, After, Rule, Ignore
}

class LongTraversableTest {
  @Test
  def map_is_lazy_on_view = {
    var check = 0

    val input = new LongTraversable[Int] {
      val data = 1 to 3
      def foreach[U](f: (Int) => U) = data foreach {i =>
        check += 1
        f(i)
      }
    }.view


    val mapped = input.map{ _ + 1}

    assertEquals(0,check)
    assertArrayEquals(Array(2,3,4),mapped.toArray)
  }

  @Test
  def limitedFold_only_processes_subset_of_elements = {
    var check = 1
    val input = new LongTraversable[Int] {
      val data = 1 to 100
      def foreach[U](f: (Int) => U) = data foreach {i =>
        check += 1
        f(i)
      }
    }

    val calculation = input.limitFold(0) {
      case (acc,i) if i < 50 => Continue(i + acc)
      case (acc,i) => End(acc + i)
    }

    val expected = 1 to 50 reduceLeft (_ + _)

    assertEquals(51,check)
    assertEquals(expected, calculation)
  }

}