import sbt._

object ExampleProjects {
	def prepare(exampleDir:File, outputDir:File) = {
		val files = exampleDir.listFiles.map{dir =>
      IO.withTemporaryDirectory {tmpDir => 
  			val files = dir.***.get.filterNot(_ == dir)
  			val mappings = files.filter(_.isFile) x relativeTo(List(exampleDir))
        mappings.foreach { case (file,relativeName) =>
          val data = IO.read(file).
            replaceAll("@SCALA_VERSION@",BuildConstants.scalaVersion).
            replaceAll("@IO_VERSION@",BuildConstants.version)
          IO.write(new File(tmpDir, relativeName), data)
        }
        zip(tmpDir, new File(tmpDir,dir.getName), outputDir)
      }
			Path.richFile(dir).relativeTo(exampleDir).get.name
		}
		val data = files.mkString("[\"","\",\"","\"]")
		IO.write(new File(outputDir, "examples.json"), data,C.utf8)
	}
	private def zip(tmpDir:File, dir:File, outDir:File) = {
		val files = dir.***.get.filterNot(_ == dir)

		val mappings = files x relativeTo(List(tmpDir))
		IO.zip(mappings, new File(outDir, dir.name+".zip"))
  }
}