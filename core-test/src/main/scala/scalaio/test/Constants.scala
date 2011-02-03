package scalaio.test

import java.io.{
InputStreamReader, BufferedReader
}
import scalax.io.Resource

object Constants {
  final lazy val IMAGE = resource("resources/image.png", getClass())

  final lazy val IMAGE_FILE_SIZE = Resource.fromInputStream(IMAGE.openStream()).byteArray.size

  final lazy val TEXT_VALUE = "1\na\nA\n\u00E0\n\u00E4\n\u00A3\n\u2248\n\u331B\n\u0060\n"

  def resource(resourceName: String, base: Class[_]) = base.getClassLoader.getResource(resourceName)
}
