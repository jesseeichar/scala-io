package scalaio.test
import org.junit.Test
import org.junit.Assert._
import scalax.test.sugar.AssertionSugar
import scalax.io.processing.Processor
import scalax.io.LongTraversable

trait TransformationTest extends AssertionSugar {
  self: LongTraversableTest =>

  @Test
  def processor_foreach_visits_each_element {
    var visitedElements = 0
    var loopCount = 0
    for {
      iter <- traversable(100, _ => visitedElements += 1).processor
      _ <- iter.repeatUntilEmpty()
      _ <- iter.next
    } {
      loopCount += 1
    }
    assertEquals(100, visitedElements)
    assertEquals(100, loopCount)
  }

  @Test
  def processor_map_is_lazy {
    var visitedElements = 0
    var loopCount = 0
    val processor = for {
      iter <- traversable(100, _ => visitedElements += 1).processor
      _ <- iter.repeatUntilEmpty()
      _ <- iter.next
    } yield {
      loopCount += 1
      loopCount
    }
    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)

    val newTraversable = processor.traversable

    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)

    newTraversable.force

    assertEquals(100, visitedElements)
    assertEquals(100, loopCount)
  }

  @Test
  def processor_flatMapping_two_tranformers_visits_each_element_once1 {
    var visitedElements1 = 0
    var visitedElements2 = 0
    var visitedElements3 = 0
    var loopCount = 0

    val traversable1 = traversable(1, _ => visitedElements1 += 1)
    val traversable2 = traversable(4, _ => visitedElements2 += 1)
    val traversable3 = traversable(2, _ => visitedElements3 += 1)
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
  }

  @Test
  def processor_flatMapping_many_tranformers_visits_each_element_once {
    var visitedElements1 = 0
    var visitedElements2 = 0
    var visitedElements3 = 0
    var visitedElements4 = 0
    var loopCount = 0

    val traversable1 = traversable(100, _ => visitedElements1 += 1)
    val traversable2 = traversable(1, _ => visitedElements2 += 1)
    val traversable3 = traversable(200, _ => visitedElements3 += 1)
    val traversable4 = traversable(100, _ => visitedElements4 += 1)
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

    val newTraversable = mappedprocessor.traversable

    assertEquals(0, visitedElements1)
    assertEquals(0, visitedElements2)
    assertEquals(0, visitedElements3)
    assertEquals(0, visitedElements4)
    assertEquals(0, loopCount)

    newTraversable.force

    assertEquals(100, visitedElements1)
    assertEquals(1, visitedElements2)
    assertEquals(200, visitedElements3)
    assertEquals(100, visitedElements4)
    assertEquals(200, loopCount)
  }

  @Test
  def processor_take {
    var visitedElements = 0
    var loops = 0
    val traversable1 = traversable(100, _ => visitedElements += 1)
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
    mappedprocessor.acquireFor(seq => assertEquals(List(1, 2, 3, 4), seq.toList))
    assertEquals(1, loops)
    assertEquals(takeNumber, visitedElements)
  }

  @Test
  def processor_drop {
    var visitedElements = 0
    var loops = 0
    val traversable1 = traversable(100, _ => visitedElements += 1)
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
    mappedprocessor.acquireFor(assertEquals(5, _))
    assertEquals(1, loops)
    assertEquals(dropNumber + 1, visitedElements)
  }

  @Test
  def processor_chaining {
    var visitedElements = 0
    var loops = 0
    val traversable1 = traversable(100, _ => visitedElements += 1)
    val dropNumber = 4
    val mappedprocessor: Processor[Int] = for {
      t1 <- traversable1.processor
      next <- t1.drop(dropNumber).next
    } yield {
      loops += 1
      next
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    mappedprocessor.acquireFor(assertEquals(5, _))
    assertEquals(1, loops)
    assertEquals(dropNumber + 1, visitedElements)
  }

  @Test
  def processor_repeat {
    var visitedElements = 0
    var loops = 0
    val traversable1 = traversable(100, _ => visitedElements += 1)
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
    assertEquals(List("1","2","3","4"), mappedprocessor.traversable.toList)
    assertEquals(repeats, loops)
    assertEquals(repeats, visitedElements)
  }
}