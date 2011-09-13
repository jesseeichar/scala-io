package scalax.io.perf
package writer

import java.io.File
import java.io.FileReader
import java.io.FileWriter

import org.apache.commons.io.FileUtils

import Utils._
import scalax.io.Line.Terminators.NewLine

trait FileBase {
  def newOut = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    () => new FileWriter(file)
  }


}