package scalaio.test
import org.junit.Test
import org.junit.Assert._
import scalax.test.sugar.AssertionSugar
import scalax.io.processing.Processor
import scalax.io.LongTraversable

trait ProcessorTest extends AssertionSugar {
  self: LongTraversableTest =>

  private def processorTraversable(elems: Int, callback: => Unit) = {
    var closed: Int = 0
    var opened: Int = 0
    new {
      val traversable = self.traversable(
        elems,
        _ => callback,
        (i: Int) => {
          opened += 1;
          1 to i
        },
        closeFunction = () => closed += 1)
      def assertOpenedClosed(times: Int) = {
        assertEquals(times, opened)
        assertEquals(times, closed)
      }
      val testData = expectedData(elems)
    }
  }
  @Test
  def processor_foreach_visits_each_element {
    var visitedElements = 0
    var loopCount = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    for {
      iter <- prepared.traversable.processor
      _ <- iter.repeatUntilEmpty()
      _ <- iter.next
    } {
      loopCount += 1
    }
    assertEquals(prepared.testData.size, visitedElements)
    assertEquals(prepared.testData.size, loopCount)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_map_is_lazy {
    var visitedElements = 0
    var loopCount = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val processor = for {
      iter <- prepared.traversable.processor
      _ <- iter.repeatUntilEmpty()
      _ <- iter.next
    } yield {
      loopCount += 1
      loopCount
    }
    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)
    prepared.assertOpenedClosed(0)

    val newTraversable = processor.traversable

    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)
    prepared.assertOpenedClosed(0)

    newTraversable.force

    assertEquals(prepared.testData.size, visitedElements)
    assertEquals(prepared.testData.size, loopCount)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_flatMapping_two_tranformers_visits_each_element_once1 {
    var visitedElements1 = 0
    var visitedElements2 = 0
    var visitedElements3 = 0
    var loopCount = 0

    val prepared1 = processorTraversable(1, visitedElements1 += 1)
    val prepared2 = processorTraversable(4, visitedElements2 += 1)
    val prepared3 = processorTraversable(2, visitedElements3 += 1)

    val traversable1 = prepared1.traversable
    val traversable2 = prepared2.traversable
    val traversable3 = prepared3.traversable

    val mappedprocessor = for {
      t1 <- traversable1.processor
      t2 <- traversable2.processor
      t3 <- traversable3.processor
      _ <- t1.repeatUntilEmpty(t2, t3)
      _ <- t1.nextOption
      _ <- t2.nextOption
      _ <- t3.nextOption
    } yield {
      loopCount += 1
      loopCount
    }
    val newTraversable = mappedprocessor.traversable

    newTraversable.force

    assertEquals(1, visitedElements1)
    assertEquals(4, visitedElements2)
    assertEquals(2, visitedElements3)
    assertEquals(4, loopCount)

    prepared1.assertOpenedClosed(1)
    prepared2.assertOpenedClosed(1)
    prepared3.assertOpenedClosed(1)

  }

  @Test
  def processor_flatMapping_many_tranformers_visits_each_element_once {
    var visitedElements1 = 0
    var visitedElements2 = 0
    var visitedElements3 = 0
    var visitedElements4 = 0
    var loopCount = 0

    val prepared1 = processorTraversable(100, visitedElements1 += 1)
    val prepared2 = processorTraversable(1, visitedElements2 += 1)
    val prepared3 = processorTraversable(200, visitedElements3 += 1)
    val prepared4 = processorTraversable(100, visitedElements4 += 1)

    val traversable1 = prepared1.traversable
    val traversable2 = prepared2.traversable
    val traversable3 = prepared3.traversable
    val traversable4 = prepared4.traversable

    val mappedprocessor = for {
      t1 <- traversable1.processor
      t2 <- traversable2.processor
      t3 <- traversable3.processor
      t4 <- traversable4.processor
      _ <- t1.repeatUntilEmpty(t2, t3, t4)
      _ <- t1.nextOption
      _ <- t2.nextOption
      _ <- t3.nextOption
      _ <- t4.nextOption
    } yield {
      loopCount += 1
      loopCount
    }
    assertEquals(0, visitedElements1)
    assertEquals(0, visitedElements2)
    assertEquals(0, visitedElements3)
    assertEquals(0, visitedElements4)
    assertEquals(0, loopCount)
    prepared1.assertOpenedClosed(0)
    prepared2.assertOpenedClosed(0)
    prepared3.assertOpenedClosed(0)
    prepared4.assertOpenedClosed(0)

    val newTraversable = mappedprocessor.traversable

    assertEquals(0, visitedElements1)
    assertEquals(0, visitedElements2)
    assertEquals(0, visitedElements3)
    assertEquals(0, visitedElements4)
    assertEquals(0, loopCount)
    prepared1.assertOpenedClosed(0)
    prepared2.assertOpenedClosed(0)
    prepared3.assertOpenedClosed(0)
    prepared4.assertOpenedClosed(0)

    newTraversable.force

    assertEquals(prepared1.testData.size, visitedElements1)
    assertEquals(prepared2.testData.size, visitedElements2)
    assertEquals(prepared3.testData.size, visitedElements3)
    assertEquals(prepared4.testData.size, visitedElements4)
    assertEquals(prepared3.testData.size, loopCount)
    prepared1.assertOpenedClosed(1)
    prepared2.assertOpenedClosed(1)
    prepared3.assertOpenedClosed(1)
    prepared4.assertOpenedClosed(1)

    newTraversable.force

    assertEquals(prepared1.testData.size * 2, visitedElements1)
    assertEquals(prepared2.testData.size * 2, visitedElements2)
    assertEquals(prepared3.testData.size * 2, visitedElements3)
    assertEquals(prepared4.testData.size * 2, visitedElements4)
    assertEquals(prepared3.testData.size * 2, loopCount)
    prepared1.assertOpenedClosed(2)
    prepared2.assertOpenedClosed(2)
    prepared3.assertOpenedClosed(2)
    prepared4.assertOpenedClosed(2)
  }

  @Test
  def processor_take {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable

    val takeNumber = 4
    val mappedprocessor: Processor[Seq[Int]] = for {
      t1 <- traversable1.processor
      taken <- t1.take(takeNumber)
    } yield {
      loops += 1
      taken
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    mappedprocessor.acquireAndGet(seq => assertEquals(prepared.testData.take(4).toList, seq.toList))
    assertEquals(1, loops)
    assertEquals(takeNumber, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_takeWhile {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val cutOff = prepared.testData.drop(4).head
    val traversable1 = prepared.traversable

    val mappedprocessor: Processor[(Seq[Int],Int)] = for {
      t1 <- traversable1.processor
      taken <- t1.takeWhile(_ != cutOff)
      next <- t1.next
    } yield {
      loops += 1
      (taken,next)
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    mappedprocessor.acquireAndGet{data => 
      assertEquals(prepared.testData.take(4).toList, data._1.toList)
      assertEquals(cutOff, data._2)
    }
    assertEquals(1, loops)
    assertEquals(5, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_drop {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val dropNumber = 4
    val mappedprocessor: Processor[Int] = for {
      t1 <- traversable1.processor
      _ <- t1.drop(dropNumber)
      next <- t1.next
    } yield {
      loops += 1
      next
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)
    mappedprocessor.acquireAndGet(assertEquals(prepared.testData.drop(dropNumber).head, _))
    assertEquals(1, loops)
    assertEquals(dropNumber + 1, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_repeat {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val repeats = 4
    val mappedprocessor: Processor[Iterator[String]] = for {
      t1 <- traversable1.processor
      _ <- t1.repeat(repeats)
      next <- t1.next
    } yield {
      loops += 1
      next.toString
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(prepared.testData.take(4).map(_.toString).toList, mappedprocessor.traversable.toList)
    assertEquals(repeats, loops)
    assertEquals(repeats, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_repeat_until {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val repeats = 4
    val mappedprocessor: Processor[Iterator[String]] = for {
      t1 <- traversable1.processor
      i <- t1.repeatUntilEmpty()
      next <- t1.next
      _ <- t1.endIf(i == repeats - 1)
    } yield {
      loops += 1
      next.toString
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(prepared.testData.take(4).map(_.toString).toList, mappedprocessor.traversable.toList)
    assertEquals(repeats, loops)
    assertEquals(repeats, visitedElements)
    prepared.assertOpenedClosed(1)
  }
  @Test
  def processor_filter {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val repeats = 4
    val mappedprocessor: Processor[Iterator[String]] = for {
      t1 <- traversable1.processor
      i <- t1.repeatUntilEmpty()
      next <- t1.next
      if i < repeats
    } yield {
      loops += 1
      next.toString
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(prepared.testData.take(4).map(_.toString).toList, mappedprocessor.traversable.toList)
    assertEquals(repeats, loops)
    assertEquals(prepared.testData.size, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_filter_traversable_mod {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val repeats = 4
    val mappedprocessor: Processor[Iterator[Int]] = for {
      t1 <- traversable1.processor
      _ <- t1.repeatUntilEmpty()
      next <- t1.next
      if next % 3 == 0
    } yield {
      loops += 1
      next
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    val expected = (prepared.testData filter (_ % 3 == 0)).toList

    assertEquals(expected, mappedprocessor.traversable.toList)
    assertEquals(expected.size, loops)
    assertEquals(prepared.testData.size, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_filter_acquireAndGet_Empty {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val mappedprocessor: Processor[String] = for {
      t1 <- traversable1.processor
      next <- t1.next
      if false
      next2 <- t1.next
    } yield {
      loops += 1
      next.toString
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    mappedprocessor.acquireAndGet(_ => ()) // will make option
    assertEquals(0, loops)
    assertEquals(1, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_filter_acquireAndGet_Value {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val mappedprocessor: Processor[String] = for {
      t1 <- traversable1.processor
      next <- t1.next
      if true
      next2 <- t1.next
    } yield {
      loops += 1
      next.toString
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    mappedprocessor.acquireAndGet(_ => ()) // will make option
    assertEquals(1, loops)
    assertEquals(2, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_for_within_for {
    var visitedElements = 0
    var visitedElements2 = 0
    var loops = 0
    val prepared = processorTraversable(25, visitedElements += 1)
    val prepared2 = processorTraversable(50, visitedElements2 += 1)

    val traversable1 = prepared.traversable
    val traversable2 = prepared2.traversable
    val mappedprocessor: Processor[Iterator[List[Int]]] = for {
      t1 <- traversable1.processor
      t2 <- traversable2.processor
      _ <- t1.repeatUntilEmpty()
      next <- t1.next
      doubled <- for {
        _ <- t2.repeat(2)
        next <- t2.next
      } yield next
    } yield {
      loops += 1
      doubled.toList
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    val iter = prepared2.testData.iterator
    assertEquals(prepared.testData map (i => List(iter.next, iter.next)) toList, mappedprocessor.traversable.toList)
    assertEquals(prepared.testData.size, loops)
    assertEquals(prepared.testData.size, visitedElements)
    assertEquals(prepared.testData.size * 2, visitedElements2)
    prepared.assertOpenedClosed(1)
    prepared2.assertOpenedClosed(1)
  }

  @Test
  def processor_assignment {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val mappedprocessor: Processor[Iterator[Int]] = for {
      t1 <- traversable1.processor
      _ <- t1.repeatUntilEmpty()
      next <- t1.next
      nextPlusOne = next + 1
    } yield {
      loops += 1
      nextPlusOne
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(prepared.testData.map(_ + 1).toList, mappedprocessor.traversable.toList)
    assertEquals(prepared.testData.size, loops)
    assertEquals(prepared.testData.size, visitedElements)
    prepared.assertOpenedClosed(1)

  }
  @Test
  def processor_extractor {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable.zipWithIndex
    val mappedprocessor: Processor[Iterator[Int]] = for {
      t1 <- traversable1.processor
      _ <- t1.repeatUntilEmpty()
      (next, index) <- t1.next
    } yield {
      loops += 1
      index
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(prepared.testData.zipWithIndex.map(_._2).toList, mappedprocessor.traversable.toList)
    assertEquals(prepared.testData.size, loops)
    assertEquals(prepared.testData.size, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_nextOption {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(1, visitedElements += 1)

    val traversable1 = prepared.traversable.take(1)
    val mappedprocessor: Processor[Iterator[Option[Int]]] = for {
      t1 <- traversable1.processor
      _ <- t1.repeat(2)
      next1 <- t1.nextOption
    } yield {
      loops += 1
      next1
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    assertEquals(List(Some(prepared.testData.head),None), mappedprocessor.traversable.toList)
    assertEquals(2, loops)
    assertEquals(1, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_LongTraversableFromIterable {
    val prepared = processorTraversable(1, ())

    val traversable = prepared.traversable
    
    val result: Processor[Seq[Int]] = for {
      api <- traversable.processor
      seq <- api.take(5)
    } yield seq
    
    assertEquals(prepared.testData.take(5).toList, result.traversable.toList)
  }
}