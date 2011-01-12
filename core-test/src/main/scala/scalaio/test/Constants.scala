package scalaio.test

import java.io.{
InputStreamReader, BufferedReader
}
import scalax.io.resource.Resource

object Constants {
  final lazy val IMAGE = resource("resources/image.png", getClass())
  final lazy val TEXT = resource("resources/text", getClass())

  final lazy val TEXT_FILE_SIZE = TEXT_VALUE.getBytes(scalax.io.Codec.UTF8.charSet)
  final lazy val IMAGE_FILE_SIZE = Resource.fromInputStream(IMAGE.openStream()).byteArray.size

  final lazy val TEXT_VALUE = {
    val reader = new BufferedReader(new InputStreamReader(TEXT.openStream, "UTF-8"))
    try {
      var line = reader.readLine()
      val value = new StringBuilder()
      while (line != null) {
        value append line
        value append "\n"
        line = reader.readLine()
      }

      value.toString
    } finally {
      reader.close()
    }
  }

  def resource(resourceName: String, base: Class[_]) = base.getClassLoader.getResource(resourceName)
}
