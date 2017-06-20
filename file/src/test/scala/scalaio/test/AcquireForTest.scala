package scalaio.test

import java.io.{FileNotFoundException, InputStream}

import org.junit.Test

import scalax.file.Path

/**
  * Test that a file can be opened and that open errors are correctly populated in the resulting Either,
  * and not rethrown, per the contract.
  */
class AcquireForTest {

  def noop(stream: InputStream): Unit = {
    // Not op'ing.
  }

  @Test
  def test(): Unit = {
    val result = Path.fromString("foobarfile").inputStream.acquireFor { noop }
    result.either match {
      case Left(seq) if seq.head.getClass == classOf[FileNotFoundException] => assert(true)
      case _                                                                => assert(false)
    }
  }
}
