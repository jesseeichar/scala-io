package scalax.io.perf

import sperformance.dsl.PerformanceDSLTest
import scalax.io.Codec
import scalax.io.Line.Terminators._
import java.io.Reader
import scalax.io.ReadChars
import scalax.io.Resource
import sperformance.Keys
import org.apache.commons.io.IOUtils
import scalax.io.JavaConverters._
abstract class AbstractReadCharsTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  def MaxSize: Int
  def Inc: Int
  def From: Int

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep): () => Reader

  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep): ReadChars = {
    val in = newIn(size, lines, term)
    Resource.fromReader(in())
  }

  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "ReadChars" in {
    having attribute (Keys.WarmupRuns -> 1) in {

      measure method "chars" in {
        withSizeDef { size =>
          newInResource(size)
        } run { in =>
          var i = 0
          in.chars.foreach(b => i += 1)
        }
      }
      measure method "chars" in {
        having attribute ("version", "java.io read chars with InputStreamReader") in {
          withSizeDef { size =>
            (size, newIn(size))
          } run {
            case (size, in) =>
              val reader = in()
              val buffer = new Array[Char](size * 2)
              var read = 0

              do {
                read = reader.read(buffer)
              } while (read > 0)
              reader.close()
          }
        }
      }
      measure method "lines newline" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, NewLine.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines newline" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)().asReadChars.chars.mkString
            scala.io.Source.fromString(in)
          } run { source =>
            var i = 0
          source.getLines().foreach(b => i += 1)
          }
        }
      }
      measure method "lines newline" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          var i = 0
          in.lines(NewLine).foreach(b => i += 1)
        }
      }
      measure method "lines Auto" in {
        withSizeDef { size =>
          newInResource(5, size, NewLine.sep)
        } run { in =>
          var i = 0
          in.lines(Auto).foreach(b => i += 1)
        }
      }
      measure method "lines Auto" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)().asReadChars.chars.mkString
            scala.io.Source.fromString(in)
          } run { source =>
            var i = 0
          source.getLines().foreach(b => i += 1)
          }
        }
      }
      measure method "lines Auto" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, NewLine.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines CR" in {
        withSizeDef { size =>
          newInResource(5, size, CarriageReturn.sep)
        } run { in =>
          var i = 0
          in.lines(CarriageReturn).foreach(b => i += 1)
        }
      }
      measure method "lines CR" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)().asReadChars.chars.mkString
            scala.io.Source.fromString(in)
          } run { source =>
            var i = 0
          source.getLines().foreach(b => i += 1)
          }
        }
      }
      measure method "lines CR" in {
        having attribute ("version", "Apache IOUtils line") in {
          withSizeDef { size =>
            newIn(5, size, CarriageReturn.sep)
          } run { inFunc =>
            val in = inFunc()
            val iter = IOUtils.lineIterator(in)
            while (iter.hasNext()) {
              iter.next()
            }
            in.close()
          }
        }
      }
      measure method "lines RN" in {
        withSizeDef { size =>
          newInResource(5, size, RNPair.sep)
        } run { in =>
          assert(in.lines(RNPair).forall { _.last != '\n' })
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "io.Source.getLines") in {
          withSizeDef { size =>
            val in = newIn(5, size, NewLine.sep)().asReadChars.chars.mkString
            scala.io.Source.fromString(in)
          } run { source =>
            var i = 0
          source.getLines().foreach(b => i += 1)
          }
        }
      }
      measure method "lines RN" in {
        having attribute ("version", "toString split on terminator") in {
          withSizeDef { size =>
            newIn(5, size, RNPair.sep)
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in).split("\r\n")
            in.close()
          }
        }
      }
      measure method "lines Custom" in {
        withSizeDef { size =>
          newInResource(5, size, "**")
        } run { in =>
          var i = 0
          in.lines(Custom("**")).foreach(b => i += 1)
        }
      }
      measure method "lines Custom" in {
        having attribute ("version", "java.io Reader") in {
          withSizeDef { size =>
            newIn(5, size, "**")
          } run { inFunc =>
            val in = inFunc()
            IOUtils.toString(in).split("\\*\\*")
            in.close()
          }
        }
      }
      measure method "chars drop" in {
        withSizeDef { size =>
          (size / 2, newInResource(size))
        } run {
          case (toDrop, in) =>
          var i = 0
          in.chars.drop(toDrop).foreach(b => i += 1)
        }
      }
      measure method "chars drop" in {
        having attribute ("version", "java.io while loop with buffer") in {
          withSizeDef { size =>
            (size, newIn(size))
          } run {
            case (size, inFunc) =>
              val in = inFunc()
              in.skip(size / 2)
              val buffer = new Array[Char](size)
              var read = in.read(buffer)
              while (read > 0) {
                var i = 0
                while (i < read) {
                  buffer(i)
                  i += 1
                }
                read = in.read(buffer)
              }
              in.close
          }
        }
      }
      measure method "chars take" in {
        withSizeDef { size =>
          (size / 2, newInResource(size))
        } run {
          case (toTake, in) =>
          var i = 0
          in.chars.take(toTake).foreach(b => i += 1)
        }
      }
      measure method "chars take" in {
        having attribute ("version", "java.io while loop with buffer") in {
          withSizeDef { size =>
            (size, newIn(size))
          } run {
            case (size, inFunc) =>
              val in = inFunc()

              val buffer = new Array[Char](size)
              val read = in.read(buffer)
              var i = 0
              while (i < read) {
                buffer(i)
                i += 1
              }
              in.close
          }
        }
      }
    }
  }

}