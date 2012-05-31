import scalax.io._
import scalax.file._
import JavaConverters._
object Main extends App {
  val tmpPath = Path.createTempFile("scala-lang-site.html")
  new java.net.URL("http://www.scala-lang.org").asInput.copyDataTo(tmpPath)

  println(tmpPath.slurpString)
  tmpPath.delete()
}
