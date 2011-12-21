package scalax.io.perf

import Utils._
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

abstract class AbstractSeekableTest extends PerformanceDSLTest {
  implicit val codec = Codec.UTF8

  def MaxSize: Int
  def Inc: Int
  def From: Int

  type Source
  def setup(size: Int,
    lines: Int = 2,
    term: String = NewLine.sep): Source
  /**
   * Return a Function that will create an input stream for testing
   * The function should not take very much time since it will be called during the test
   * and if it does it could interfere with what the test measures.
   *
   * For example newIn could create a file and the function would simply open a stream to the file
   */
  def newIn(source: Source,
    openOptions: Seq[OpenOption] = ReadWrite): () => SeekableByteChannel

  def newInResource(source: Source, openOptions: Seq[OpenOption] = ReadWrite): Seekable = {
    val seekable = newIn(source, openOptions)
    fromSeekableByteChannel(seekable())

  }

  def withSizeDef[U](f: Int => U) = withSize from (From) upTo MaxSize by Inc withSetup (f)

  performance of "Seekable" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "patch strings" in {
        withSizeDef { size =>
          (size, size / 2, generateTestData(size, 1))
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "patch bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data.length, data)
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "patch bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data.length, data.toList)
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.patch(pos, data, OverwriteAll)
        }
      }
      measure method "append strings" in {
        withSizeDef { size =>
          (size, generateTestData(size, 1))
        } run {
          case (size, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.append(data)
        }
      }
      measure method "append bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data)
        } run {
          case (size, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.append(data)
        }
      }
      measure method "append bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data.toList)
        } run {
          case (size, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.append(data)
        }
      }
      measure method "insert strings" in {
        withSizeDef { size =>
          (size, size / 2, generateTestData(size, 1))
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.insert(pos, data)
        }
      }
      measure method "insert bytes array" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data.length / 2, data)
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.insert(pos, data)
        }
      }
      measure method "insert bytes list" in {
        withSizeDef { size =>
          val data = generateTestData(size, 1).getBytes(Codec.UTF8.name)
          (size, data.length / 2, data.toList)
        } run {
          case (size, pos, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.insert(pos, data)
        }
      }
      measure method "bytes drop" in {
        withSizeDef { size =>
          (size)
        } run {
          case (size) =>
            val source = setup(size)
            val seekable = newInResource(source)
            val f = new CountFunction[Byte]
            seekable.bytes.drop(size / 2).foreach(f)
        }
      }
      measure method "bytes take" in {
        withSizeDef { size =>
          (size)
        } run {
          case (size) =>
            val source = setup(size)
            val seekable = newInResource(source)
            val f = new CountFunction[Byte]
            seekable.bytes.take(size / 2).foreach(f)
        }
      }
      measure method "bytes zip" in {
        withSizeDef { size =>
          val data = generateTestData(size).getBytes(Codec.UTF8.name)
          (size, data.toList)
        } run {
          case (size, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            val f = new CountFunction[(Byte,Byte)]
            seekable.bytes.zip(data).foreach(f)
        }
      }
      measure method "bytes limitFold" in {
        withSizeDef { size =>
          val data = generateTestData(size).getBytes(Codec.UTF8.name)
          (size, data.toList)
        } run {
          case (size, data) =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.bytes.limitFold(0) { (acc, next) =>
              if (acc < size / 2) Continue(acc + 1)
              else End(acc)
            }
        }
      }
      measure method "bytes apply" in {
        having attribute ("version", "std nio") in {
          withSizeDef { size =>
            size
          } run { size =>
            val source = setup(size)
            val seekable = newInResource(source)
            seekable.bytes(0)
          }
        }
      }
    }
  }
}