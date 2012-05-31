import java.net.URL
import scalax.io.JavaConverters._
import scalax.file.Path

object ScalaIOExample extends App {
  val scalalang = new URL("http://www.scala-lang.org").asInput.bytes
  val scalatools = new URL("http://www.scala-tools.org").asInput.bytes

  Path("scalaout").write(scalalang ++ scalatools)
}
