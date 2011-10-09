import sbt._

object ExampleProjects {
	def prepare(exampleDir:File, outputDir:File) = {
		val files = exampleDir.listFiles.map{dir =>
			val files = dir.***.get.filterNot(_ == dir)

			val mappings = files x relativeTo(List(dir))
			IO.zip(mappings, new File(outputDir, dir.name+".zip"))
			Path.richFile(dir).relativeTo(exampleDir).get.name
		}
		val data = files.mkString("[\"","\",\"","\"]")
		IO.write(new File(outputDir, "examples.json"), data,C.utf8)
	}
}