package scalaio.test
import org.junit.Test
import org.junit.Assert._
import scalax.test.sugar.AssertionSugar

trait TransformationTest extends AssertionSugar {
  self: LongTraversableTest =>

  @Test
  def transformer_foreach_visits_each_element {
    var visitedElements = 0
    var loopCount = 0
    for (iter <- traversable(100, _ => visitedElements += 1).transformer) {
      iter.next
      loopCount += 1

    }
    assertEquals(100, visitedElements)
    assertEquals(100, loopCount)
  }

  @Test
  def transformer_map_is_lazy {
    var visitedElements = 0
    var loopCount = 0
    val transformer = for (iter <- traversable(100, _ => visitedElements += 1).transformer) yield {
      iter.next
      loopCount += 1
      loopCount
    }
    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)

    val newTraversable = transformer.traversable

    assertEquals(0, visitedElements)
    assertEquals(0, loopCount)

    newTraversable.force

    assertEquals(100, visitedElements)
    assertEquals(100, loopCount)
  }

  @Test
  def transformer_flatMapping_two_tranformers_visits_each_element_once1 {
    var visitedElements1 = 0
    var visitedElements2 = 0
    var visitedElements3 = 0
    var loopCount = 0
    
    val traversable1 = traversable(1, _ => visitedElements1 += 1)
    val traversable2 = traversable(4, _ => visitedElements2 += 1)
    val traversable3 = traversable(2, _ => visitedElements3 += 1)
    val mappedTransformer = for {
      t1 <- traversable1.transformer
      t2 <- traversable2.transformer
      t3 <- traversable3.transformer
    } yield {
      loopCount += 1
      if(t1.hasNext) t1.next()
      if(t2.hasNext) t2.next()
      if(t3.hasNext) t3.next()
      loopCount
    }
    val newTraversable = mappedTransformer.traversable

    newTraversable.force

    assertEquals(1, visitedElements1)
    assertEquals(4, visitedElements2)
    assertEquals(2, visitedElements3)
    assertEquals(4, loopCount)
  }
  
  @Test
  def transformer_flatMapping_two_tranformers_visits_each_element_once {
      var visitedElements1 = 0
              var visitedElements2 = 0
              var visitedElements3 = 0
              var visitedElements4 = 0
              var loopCount = 0
              
              val traversable1 = traversable(100, _ => visitedElements1 += 1)
              val traversable2 = traversable(1, _ => visitedElements2 += 1)
              val traversable3 = traversable(200, _ => visitedElements3 += 1)
              val traversable4 = traversable(100, _ => visitedElements4 += 1)
              val mappedTransformer = for {
                  t1 <- traversable1.transformer
                  t2 <- traversable2.transformer
                  t3 <- traversable3.transformer
                  t4 <- traversable4.transformer
              } yield {
                  loopCount += 1
                          if(t1.hasNext) t1.next()
                          if(t2.hasNext) t2.next()
                          if(t3.hasNext) t3.next()
                          if(t4.hasNext) t4.next()
                          loopCount
              }
              assertEquals(0, visitedElements1)
              assertEquals(0, visitedElements2)
              assertEquals(0, visitedElements3)
              assertEquals(0, visitedElements4)
              assertEquals(0, loopCount)
              
              val newTraversable = mappedTransformer.traversable
              
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
              assertEquals(100, loopCount)
  }

}