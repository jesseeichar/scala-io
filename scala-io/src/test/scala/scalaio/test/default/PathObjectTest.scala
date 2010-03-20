/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.default

import org.junit.Assert._
import org.junit.{Test,Ignore}
import scalax.io._
import scalaio.test.ScalaIoMocks.fileSystemMock

class PathObjectTest {
  @Test
  def path_object_should_implicitly_create_path_from_string(): Unit = {
    import Path.string2path
    
    {assertSame(FileSystem.default, "nonsense path".fileSystem) }
    
    {
      implicit val fs:FileSystem = fileSystemMock
      val p:Path = "hi"
      assertSame(fs, "path".fileSystem)
    }
  }
    
  @Test
  def path_object_should_implicitly_create_path_from_a_java_file() : Unit = {
    import java.io.File
    import Path._

    {assertSame(FileSystem.default, new File("nonsense path").fileSystem)}
    {
      implicit val fs = fileSystemMock
      assertSame(fs, new File("path").fileSystem)
    }
  }
  
  @Test
  def path_object_should_create_paths_from_a_string() : Unit = {
    {assertSame(FileSystem.default, Path("nonsense path").fileSystem)}
    {
      implicit val fs = fileSystemMock
      assertSame(fs, Path("path").fileSystem)
    }
  }
}
