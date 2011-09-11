package scalax.io.perf

import collection.mutable.ArrayBuffer
import util.Random._
import sperformance.dsl.PerformanceDSLTest
import sperformance.Keys
import scalax.io.Input
import java.nio.channels.ReadableByteChannel
import scalax.io._
import Resource._
import StandardOpenOption._
import Line.Terminators._
import scalax.file._
import defaultfs.DefaultPath
import support.FileUtils
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import nio.SeekableFileChannel

trait AbstractSeekableTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  def MaxSize: Int
  def Inc: Int
  def From: Int
  def WarmUpRuns: Int

  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(size: Int, 
      lines: Int = 2, 
      term: String = NewLine.sep, 
      openOptions: Seq[OpenOption] = ReadWrite) = () => {
    val buffer = ArrayBuffer(generateTestData(size, lines, term).getBytes(Codec.UTF8.name):_*)
    new ArrayBufferSeekableChannel(buffer,openOptions:_*)((),()):SeekableByteChannel
  }
  def newInResource(size: Int, lines: Int = 2, term: String = NewLine.sep, openOptions: Seq[OpenOption] = ReadWrite): Seekable = {
    val seekable = newIn(size, lines, term)
    fromSeekableByteChannel(seekable())

  }
  def generateTestData(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val lineStrings = 1 to lines map { _ =>
      (1 to size map { _ => nextPrintableChar() }).mkString.replaceAll("\n", " ")
    }
    lineStrings mkString term
  }

  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "Seekable" in {
    having attribute (Keys.WarmupRuns -> WarmUpRuns) in {
      measure method "patch strings" in {
        withSizeDef { size =>
          (size / 2, generateTestData(size, 1), newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "patch bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data.length, data, newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "patch bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data.length, data.toList, newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "append strings" in {
        withSizeDef { size =>
          (generateTestData(size, 1), newInResource(size))
        } run {
          case (data, seekable) =>
            seekable.append(data)
        }
      }
      measure method "append bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data, newInResource(size))
        } run {
          case (data, seekable) =>
            seekable.append(data)
        }
      }
      measure method "append bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data.toList, newInResource(size))
        } run {
          case (data, seekable) =>
            seekable.append(data)
        }
      }
      measure method "insert strings" in {
        withSizeDef { size =>
          (size / 2, generateTestData(size, 1), newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.insert(pos, data)
        }
      }
      measure method "insert bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data.length / 2, data, newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.insert(pos, data)
        }
      }
      measure method "insert bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (data.length / 2, data.toList, newInResource(size))
        } run {
          case (pos, data, seekable) =>
            seekable.insert(pos, data)
        }
      }
    }
  }
}