package scalaio.test
import org.junit.Test
import org.junit.Assert._
import scalax.test.sugar.AssertionSugar
import scalax.io.processing.{ProcessorFactory, Processor, CharProcessor, ByteProcessor}
import java.util.concurrent.TimeoutException
import scalax.io.{Resource, LongTraversable, ResourceContext, DefaultResourceContext}
import java.io.{File, DataInputStream, ByteArrayInputStream}
import akka.dispatch.Await
import akka.util.duration._

trait ProcessorTest extends AssertionSugar {
  self: LongTraversableTest =>

  private def processorTraversable(elems: Int, callback: => Unit, context:ResourceContext = DefaultResourceContext) = {
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
        closeFunction = () =>
          closed += 1,
        resourceContext = context)
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
      _ <- t1.next.opt
      _ <- t2.next.opt
      _ <- t3.next.opt
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
      _ <- t1.next.opt
      _ <- t2.next.opt
      _ <- t3.next.opt
      _ <- t4.next.opt
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
    val mappedprocessor: Processor[LongTraversable[String]] = for {
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
  def processor_repeatUntilEmpty_endIf {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(100, visitedElements += 1)

    val traversable1 = prepared.traversable
    val repeats = 4
    val mappedprocessor: Processor[LongTraversable[String]] = for {
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
    val mappedprocessor: Processor[LongTraversable[String]] = for {
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
    val mappedprocessor: Processor[LongTraversable[Int]] = for {
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
    val mappedprocessor: Processor[LongTraversable[List[Int]]] = for {
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
    val mappedprocessor: Processor[LongTraversable[Int]] = for {
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
    val mappedprocessor: Processor[LongTraversable[Int]] = for {
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
  def processor_next_opt {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(1, visitedElements += 1)

    val traversable1 = prepared.traversable.take(1)
    val mappedprocessor: Processor[(Option[Int],Option[Int])] = for {
      t1 <- traversable1.processor
      next1 <- t1.next.opt
      next2 <- t1.next.opt
    } yield {
      loops += 1
      (next1,next2)
    }
    assertEquals(0, loops)
    assertEquals(0, visitedElements)
    prepared.assertOpenedClosed(0)

    mappedprocessor.acquireAndGet(assertEquals((Some(prepared.testData.head),None), _))
    assertEquals(1, loops)
    assertEquals(1, visitedElements)
    prepared.assertOpenedClosed(1)
  }

  @Test
  def processor_LongTraversableFromIterable {
    val prepared = processorTraversable(100, ())

    val traversable = prepared.traversable

    val result: Processor[Seq[Int]] = for {
      api <- traversable.processor
      seq <- api.take(5)
    } yield seq

    assertEquals(prepared.testData.take(5).toList, result.traversable.toList)
  }

  @Test
  def processor_lines {
    val prepared = processorTraversable(100, ())

    val traversable = prepared.traversable.map(i => if(i == 9) '\n' else i.toString.charAt(0))

    val result: Processor[Seq[Char]] = for {
      api <- CharProcessor(traversable.processor)
      line <- api.line()
    } yield line

    assertEquals(prepared.testData.takeWhile(_ != 9).map(_.toString.charAt(0)).toList, result.traversable.toList)
  }


  @Test
  def processor_byte_processor {
    val prepared = processorTraversable(100, ())

    val dataFunction = (i:Int) => i.toByte
    val traversable = prepared.traversable.map(dataFunction)
    val testData = prepared.testData.map(dataFunction).toArray

    val result = for {
      api <- ByteProcessor(traversable.processor)
      short <- api.nextShort
      int <- api.nextInt
      long <- api.nextLong
    } yield (short, int,long)
    val dataIn = new DataInputStream(new ByteArrayInputStream(testData))
    val expectedShort = dataIn.readShort()
    val expectedInt = dataIn.readInt()
    val expectedLong = dataIn.readLong()
    result.acquireAndGet{
      case (short, int,long) =>
      assertEquals(expectedShort,short)
        assertEquals(expectedInt,int)
        assertEquals(expectedLong,long)
    }
  }

  object ByteConverter {
    def swap(value: Short) = {
      val b1 = value & 0xff;
      val b2 = (value >> 8) & 0xff;

      (b1 << 8 | b2 << 0).asInstanceOf[Short];
    }

    def swap(value: Int) = {
      val b1 = (value >> 0) & 0xff;
      val b2 = (value >> 8) & 0xff;
      val b3 = (value >> 16) & 0xff;
      val b4 = (value >> 24) & 0xff;

      b1 << 24 | b2 << 16 | b3 << 8 | b4 << 0;
    }

    def swap(value: Long): Long = {
      val b1 = ((value >> 0) & 0xff).asInstanceOf[Long];
      val b2 = ((value >> 8) & 0xff).asInstanceOf[Long];
      val b3 = ((value >> 16) & 0xff).asInstanceOf[Long];
      val b4 = ((value >> 24) & 0xff).asInstanceOf[Long];
      val b5 = ((value >> 32) & 0xff).asInstanceOf[Long];
      val b6 = ((value >> 40) & 0xff).asInstanceOf[Long];
      val b7 = ((value >> 48) & 0xff).asInstanceOf[Long];
      val b8 = ((value >> 56) & 0xff).asInstanceOf[Long];

      b1 << 56 | b2 << 48 | b3 << 40 | b4 << 32 | b5 << 24 | b6 << 16 | b7 << 8 | b8 << 0;
    }

    def swap(value: Float): Float = {
      val intValue = swap(java.lang.Float.floatToIntBits(value));

      java.lang.Float.intBitsToFloat(intValue);
    }

    def swap(value: Double): Double = {
      val longValue = swap(java.lang.Double.doubleToLongBits(value));
      return java.lang.Double.longBitsToDouble(longValue);
    }

  }

  @Test
  def processor_little_endian_byte_processor {
    val prepared = processorTraversable(100, ())

    val dataFunction = (i: Int) => i.toByte
    val traversable = prepared.traversable.map(dataFunction)
    val testData = prepared.testData.map(dataFunction).toArray

    val result = for {
      api <- ByteProcessor(traversable.processor)
      littleEndianAPI = api.littleEndianAPI
      short <- littleEndianAPI.nextShort
      int <- littleEndianAPI.nextInt
      long <- littleEndianAPI.nextLong
    } yield (short, int, long)

    val dataIn = new DataInputStream(new ByteArrayInputStream(testData))
    val expectedShort = ByteConverter.swap(dataIn.readShort())
    val expectedInt = ByteConverter.swap(dataIn.readInt())
    val expectedLong = ByteConverter.swap(dataIn.readLong())
    result.acquireAndGet {
      case (short, int, long) =>
        assertEquals(expectedShort, short)
        assertEquals(expectedInt, int)
        assertEquals(expectedLong, long)
    }
  }

  @Test
  def processor_using_embedded_long_traversable {
    var visitedElements = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    var visitedElements2 = 0
    val prepared2 = processorTraversable(4*10, visitedElements2 += 1)

    val p = for {
      p1 <- prepared.traversable.processor
      p2 <- prepared2.traversable.processor
      _ <- p1.repeatUntilEmpty()
      v <- p1.next
      _ <- p2.repeat(4)
      v2 <- p2.next
    } yield {
      (v,v2)
    }

    assertEquals(0, visitedElements)
    assertEquals(0, visitedElements2)
    prepared.assertOpenedClosed(0)
    prepared2.assertOpenedClosed(0)

    p.execute()

    prepared.assertOpenedClosed(1)
    prepared2.assertOpenedClosed(1)
    assertEquals(4, visitedElements)
    assertEquals(4*4, visitedElements2)
    visitedElements = 0
    visitedElements2 = 0

    p.acquireAndGet{lt =>
      lt.foreach {ll => ll.foreach(_ => ())}
    }

    assertEquals(4, visitedElements)
    assertEquals(4*4, visitedElements2)
    prepared.assertOpenedClosed(2)
    prepared2.assertOpenedClosed(2)
    visitedElements = 0
    visitedElements2 = 0

    val lt = p.traversable
    lt.head.foreach(_ => ())

    assertEquals(1, visitedElements)
    assertEquals(4, visitedElements2)
    prepared.assertOpenedClosed(3)
    prepared2.assertOpenedClosed(3)
    visitedElements = 0
    visitedElements2 = 0

  }

  @Test
  def processor_transfersContext = {
    val customContext = new ResourceContext{}
    val prepared = processorTraversable(100, () => (), context = customContext)

    val p = for {
      iter <- prepared.traversable.processor
      _ <- iter.repeatUntilEmpty()
      n <- iter.next
    } yield n
    assertSame(customContext, p.traversable.context)
  }

  @Test
  def processor_errorHandler {
    var visitedElements = 0
    var loops = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    val traversable:LongTraversable[Int] = prepared.traversable.map {
      case 1 => throw new Error("Should be caught")
      case i => i
    }
    for {
      p <- traversable.processor
      _ <- p.repeatUntilEmpty()
      byte <- p.next.onFailure{case _ => Some(1)}
    } {
      loops += 1
    }

    assertEquals(prepared.testData.size, loops)
  }

  @Test
  def processor_exception_will_propagate_up {
    var visitedElements = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    val traversable: LongTraversable[Int] = prepared.traversable.map {
      case _ => throw new Error("Should be caught")
    }
    intercept[MyException] {
      for {
        p <- traversable.processor
        byte <- p.next.onFailure {
          case _ => throw new MyException()
        }
      } {}
    }
    intercept[MyException] {
      val p = for {
        p <- traversable.processor
        byte <- p.next.onFailure {
          case _ => throw new MyException()
        }
      } yield {}
      p.execute()
    }
  }

  @Test
  def processor_parent_can_catch_exception_raised_by_child_processor_handler {
    var visitedElements = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    val traversable: LongTraversable[Int] = prepared.traversable.map {
      case _ => throw new Error("Should be caught")
    }
    intercept[ParentException] {
      for {
        p <- traversable.processor
        p2 = for{byte <- p.next.onFailure { case _ => throw new MyException()}} yield byte
        byte <- p2.onFailure{case _:MyException => throw new ParentException()}
      } {}
    }
    intercept[ParentException] {
      val p = for {
        p <- traversable.processor
        p2 = for{byte <- p.next.onFailure { case _ => throw new MyException()}} yield byte
        byte <- p2.onFailure{case _:MyException => throw new ParentException()}
      } yield {}

      p.execute()
    }
  }

  @Test
  def processor_parent_can_catch_exception_raised_during_child_processor_execution {
    var visitedElements = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    val traversable: LongTraversable[Int] = prepared.traversable.map {
      case _ => throw new Error("Should be caught")
    }
    class ParentException extends RuntimeException
    intercept[ParentException] {
      for {
        p <- traversable.processor
        p2 = for{byte <- p.next} yield byte
        byte <- p2.onFailure{case _ => throw new ParentException()}
      } {}
    }
    intercept[ParentException] {
      val p = for {
        p <- traversable.processor
        p2 = for{byte <- p.next} yield byte
        byte <- p2.onFailure{case _ => throw new ParentException()}
      } yield {}

      p.execute()
    }
  }

  @Test
  def processor_containing_filter_parent_can_catch_exception_raised_during_child_processor_execution {
    var visitedElements = 0
    val prepared = processorTraversable(4, visitedElements += 1)
    val traversable: LongTraversable[Int] = prepared.traversable.map {
      case i if i > 1 => throw new Error("Should be caught")
    }
    intercept[ParentException] {
      for {
        p <- traversable.processor
        p2 = for {
          b1 <- p.next
          if true
          b2 <- p.next
        } yield b2
        byte <- p2.onFailure{case _ => throw new ParentException()}
      } {}
    }
    intercept[ParentException] {
      val p = for {
        p <- traversable.processor
        p2 = for {
          b1 <- p.next
          if true
          b2 <- p.next
        } yield b2
        byte <- p2.onFailure{case _ => throw new ParentException()}
      } yield {}

      p.execute()
    }
  }
  class MyException extends RuntimeException
  class ParentException extends RuntimeException

  @Test
  def processor_with_error_handling_must_pass_resource_context {
    val customContext = new ResourceContext{}
    val prepared = processorTraversable(100, () => (), context = customContext)


    val p = for {
      api <- prepared.traversable.processor.onFailure{case _ => throw new RuntimeException("boom")}
      _ <- api.repeatUntilEmpty()
      i <- api.next
    } yield i

    assertSame(customContext, p.traversable.context)
    assertSame(customContext, (p.onFailure{case _ => throw new RuntimeException("boom")}).traversable.context)
  }

  @Test
  def processor_future_must_process_long_traversables {
    val prepared = processorTraversable(100, () => ())
    val out = Resource.fromFile(File.createTempFile("scalaio","xx"))

    val p = for {
      in <- prepared.traversable.processor
      outApi <- out.outputProcessor
      _ <- in.repeatUntilEmpty()
      nextInt <- in.next
      _ <- outApi.write(nextInt.toString)
    } yield ()

    Await.result(p.futureExec(), 30 hours)

    assertEquals(prepared.testData.mkString, out.slurpString)
  }

}
