package scalax.io

import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}

class SmallSetsInMemoryInputTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  val MaxSize = 50
  val Inc = 2

  def newIn(size:Int, lines:Int = 2, term:String = NewLine.sep) = {
    val lineStrings = 1 to lines map {_ =>
      nextString(size)
    }
    val data = lineStrings mkString term
    fromInputStream(new ByteArrayInputStream(data.getBytes))
  }

  //using baseLineVersion "0.1.1"

  performance of "Input" in {
    measure method "bytes" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(size)
      } run { in =>
        in.bytes.force
      }
    }
    measure method "bytesAsInts" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(size)
      } run { in =>
        in.bytesAsInts.force
      }
    }
  }
  /*
  performance of "Input" in {
    measure method "byteArray" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(size)
      } run { in =>
        in.byteArray
      }
    }
  }
  performance of "Input" in {
    measure method "copyData" in {
      withSize upTo MaxSize by Inc withSetup { size =>
      val in = newIn(size)
      val out = fromOutputStream(new ByteArrayOutputStream())
        (in,out)
      } run { case (in,out) =>
        in.copyData(out)
      }
    }
  }
  */
  performance of "Input" in {
    measure method "chars" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(size)
      } run { in =>
        in.chars.force
      }
    }
  }
  /*
  performance of "Input" in {
    measure method "lines newline" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(5,size,NewLine.sep)
      } run { in =>
        in.lines(Line.Terminators.NewLine).force
      }
    }
  }
  performance of "Input" in {
    measure method "lines Auto" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(5,size,NewLine.sep)
      } run { in =>
        in.lines(Line.Terminators.Auto()).force
      }
    }
  }
  performance of "Input" in {
    measure method "lines CR" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(5,size,CarriageReturn.sep)
      } run { in =>
        in.lines(CarriageReturn).force
      }
    }
  }
  performance of "Input" in {
    measure method "lines RN" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(5,size,RNPair.sep)
      } run { in =>
        in.lines(CarriageReturn).force
      }
    }
  }
  performance of "Input" in {
    measure method "lines Custom" in {
      withSize upTo MaxSize by Inc withSetup { size =>
        newIn(5,size,"**")
      } run { in =>
        in.lines(Custom("**")).force
      }
    }
  }*/
}