package scalax.file.ramfs

import java.util.Iterator
import java.nio.file.DirectoryStream
import scalax.file.NotDirectoryException
import java.nio.file.Path

class RamDirectoryStream(path: RamPath, filter: DirectoryStream.Filter[_ >: Path]) extends DirectoryStream[Path] {
  
  def lookup = {
    val node = path.getFileSystem.lookup(path).getOrElse(throw new NotDirectoryException("Directory "+path+" does not exist"))
    node match {
      case dir: DirNode => dir
      case _ => throw new NotDirectoryException("Path "+path+" does reference a directory")
    }
  }
  
  def close() = ()
  
  override lazy val iterator = new Iterator[Path] {
    val inner = lookup.children.iterator
    var n:RamPath = null

    def loadNext: Boolean = {
      val p = path resolve inner.next.name
      if (filter.accept(p)) {
        n = p
      } else if (inner.hasNext){
        loadNext
      }
      
      n != null
    } 
    def hasNext = 
      if(n != null) {true}
      else if(!inner.hasNext) {false}
      else loadNext
    def next = {
        if (!hasNext) throw new NoSuchElementException()
        val t = n
        n = null
        t
      }
    
    def remove = throw new UnsupportedOperationException()
  }
}