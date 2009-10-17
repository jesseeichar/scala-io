import scala.io._
Path("src").toDirectory
             .deepList(100)
               .filter( _.name.endsWith(".scala"))
               .foreach ( f => {
                          val data = f.toFile.slurp
                           f.toFile.writeAll(List(data), codec=Codec.UTF)
                        })