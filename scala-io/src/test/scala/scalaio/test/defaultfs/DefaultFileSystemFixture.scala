/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.defaultfs

import util.Random

import scalax.io._
import scalax.io.resource._

import org.junit.rules.TemporaryFolder
import java.io.InputStream
import scalaio.test._

class DefaultFileSystemFixture(val folder : TemporaryFolder, rnd : Random = new Random())
  extends FileSystemFixture(FileSystem.default, rnd) {
    folder.create()

    override val root = Path(folder.getRoot)
    override def after = folder.delete()

    /**
     * Copy resource from test resources to filesystem
     */
    def copyResource(source : InputStreamResource[InputStream]) : Path = {
        val dest = path
        dest.ops write (source.bytes)
        dest
    }
    override def text(sep:String) = {
        val resource = Resource.fromInputStream {
            val bytes = Constants.TEXT_VALUE.replaceAll("""\n""", sep).getBytes(Codec.UTF8.name)
            new java.io.ByteArrayInputStream(bytes)
        }
        copyResource(resource)
    }
    override def image = copyResource(Resource.fromInputStream(Constants.IMAGE.openStream))
}
