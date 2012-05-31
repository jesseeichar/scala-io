package scalax.io.perf
package reader

import java.io.File
import java.io.FileReader
import java.io.FileWriter

import org.apache.commons.io.FileUtils

import Utils._
import scalax.io.Line.Terminators.NewLine

trait FileBase {
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    val data = generateTestData(size, lines, term)
    FileUtils.writeStringToFile(file, data, "UTF-8")
    () => new FileReader(file)
  }
  def newOut = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    () => new FileWriter(file)
  }


}
