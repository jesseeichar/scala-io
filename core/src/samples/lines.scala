import java.io.File

/**
 * Traversing lines in Input or ReadChars.
 * <p>
 * Both Input and ReadChars objects have a method traversing the object one line at a time.
 * The terminator can be autodetected (which assumes the ending is one of \n, \r\n, \r) or it
 * can be explicitely declared, or it can be a custom separator such as <em>';'</em>.
 * </p><p>
 * Note: The <em>lines()</em> method is lazily evaluated so calling the method without processing will
 * not result in any processing of the resource</p>
 */
object LinesExamples {
  /**
   * Default behaviour.  Autodetect ending assuming one of
   * \n, \r\n or \r.  The terminator is not included in results
   */
  def linesDefaults {
    import scalax.io._

    val lines = Resource.fromFile("file").lines()
    println(lines.size)
  }
  /**
   * Explicitly declare line ending as AutoDetect and include terminator
   */
  def linesAutoIncludeTerminators {
    import scalax.io._
    import Line.Terminators.Auto

    val lines = Resource.fromFile("file").lines(Auto(),true)
    println(lines.size)
  }
  /**
   * Explicitly declare line ending as NewLine and <em>do not</em> include terminator
   */
  def linesNewLineTerminator {
    import scalax.io._
    import Line.Terminators.NewLine

    val lines = Resource.fromFile("file").lines(NewLine,false)
    println(lines.size)
  }
  /**
   * Explicitly declare a custom line terminator and <em>do not</em> include terminator
   */
  def linesCustomTerminator {
    import scalax.io._
    import Line.Terminators.Custom

    val readChars:ReadChars = Resource.fromFile("file").reader
    val lines = readChars.lines(Custom("**"))
    println(lines.size)
  }
}
