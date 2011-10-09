import scalax.io._
import JavaConverters._
object Main extends App {  
  println(new java.net.URL("http://www.scala-lang.org").asInput.slurpString(Codec.UTF8))
}