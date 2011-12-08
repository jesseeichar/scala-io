/*                      __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
package ramfs

import java.net.{
  URI, URLStreamHandler
}
import java.util.UUID
import java.io.{IOException, FileNotFoundException}

object RamFileSystem {
  case class RamFsId(id:String = UUID.randomUUID.toString)
  val protocol = "ramfs"
  private val fileSystems = scala.collection.mutable.WeakHashMap[RamFsId,RamFileSystem]()
  def apply(separator:String = "/") : RamFileSystem = new RamFileSystem(separator = separator)
  def apply(fsId:RamFsId) : RamFileSystem = synchronized {
    fileSystems.get(fsId).getOrElse(new RamFileSystem(fsId))
  }
  def apply(uri:URI) : RamPath = {
    require(uri.toString contains '!', "Ramfile system URIs must be of form: ramfs://fsId!path, was: "+uri+" (did not contain a !)")
    require(uri.getScheme equalsIgnoreCase "ramfs", "Ramfile system URIs must start with ramfs, was: "+uri)

    val id = RamFsId(uri.getAuthority.takeWhile{_ != '!'})
    val fs = apply(id)
    val path = uri.getRawPath.replace("/", fs.separator)
    fs.fromString(path)
  }
  private def register(fsId:RamFsId, fs:RamFileSystem) = synchronized {
    fileSystems(fsId) = fs
  }
}

class RamFileSystem(val id : RamFileSystem.RamFsId = RamFileSystem.RamFsId(), val separator:String = "/") extends FileSystem {
  private var fsTree = new DirNode(separator)

  RamFileSystem.register(id,this)

  val root = new RamPath("",fsTree.name, this)
  var pwd = root

  val name = "Ram ("+id+")"
  override lazy val urlStreamHandler : Option[URLStreamHandler] = Some(Handler)
  def fromString(path: String): RamPath = {
    if(path startsWith separator) fromStrings("",path)
    else fromStrings(pwd.toAbsolute.path, path)
  }

  def apply(relativeTo:String, segments: Seq[String]): RamPath = fromStrings(relativeTo,segments.filterNot{_.isEmpty} mkString separator)

  protected[ramfs] def fromStrings(relativeTo:String , path: String): RamPath = {
    def process(path:String) = {
//      println("regex",java.util.regex.Pattern.quote(separator))
      import java.util.regex.Pattern.quote
      val p = path.replace(separator+separator, separator);
      if((p endsWith separator) && (p.length > 1)) p.drop(1)
      else p
    }
    val newpath = new RamPath(process(relativeTo), process(path), this)
    if(newpath == root) root
    else newpath
  }
  override def roots:Set[Path] = Set (root)
  
  def createTempFile(prefix: String = randomPrefix,
                   suffix: String = null,
                   dir: String = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = apply(separator,"temp",UUID.randomUUID.toString)

  def createTempDirectory(prefix: String = randomPrefix,
                        suffix: String = null,
                        dir: String = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path  = apply(separator,"temp",UUID.randomUUID.toString)

  def uri(path:RamPath = root):URI = new URI(RamFileSystem.protocol+"://"+id.id+"!"+path.path.replaceAll("\\\\","/"))
  override def toString = "Ram File System"

  private[ramfs] def lookup(path:RamPath) = {
    val absolutePath = path.toAbsolute.segments
    fsTree.lookup(absolutePath)
  }
  private[ramfs] def create(path:RamPath, fac:NodeFac, createParents:Boolean = true) : Boolean = {
    if (path == root) {
      true
    } else {
      val absolute = path.toAbsolute
      absolute.parent match {
        case Some(p) if p.nonExistent && !createParents =>
          throw new FileNotFoundException("Parent directory " + p + " does not exist")
        case _ => ()
      }

      val x = fsTree.create(absolute.segments.drop(1), fac)

      true
    }
  }
  private[ramfs] def delete(path:RamPath, force:Boolean) : Boolean = {
    if(path.exists) {
      def delete(p:Path) = force || (p.canWrite && p.parent.forall {_.canWrite})

      if(delete(path) && path != root) {
        val parentPath = path.toAbsolute.parent
        val deletions = for { parent <- path.toAbsolute.parent
              parentNode <- parent.node
              node <- path.node
            } yield {
              parentNode.asInstanceOf[DirNode].children -= node
              true
            }
        deletions.isDefined
      } else if(path == root) {
        fsTree = new DirNode(separator)
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  private[ramfs] def move(src:RamPath, dest:RamPath) = {
    if(src == root) {
      throw new IOException("Root cannot be moved")
    }
    val parentNode =
      dest.parent match {
        case Some(`root`) | None =>
          fsTree
        case Some(parent) =>
          create(parent, DirNode, true) // TODO paramaterize NodeFactory
          parent.node.get.asInstanceOf[DirNode]
      }

    src.node foreach { node =>
      node.name = dest.name
      parentNode.children += node
    }

    delete(src,true)
  }

  /**
   * creates and copies the data of the src node to the destination.
   * Assumption is the destination does not exist
   */
   private[ramfs] def copyFile(src:RamPath, srcNode:FileNode, dest:RamPath) = {

     dest.fileSystem.create(dest, FileNode, true)
     val newNode = dest.node.collect{case newNode:FileNode =>
       newNode.data = srcNode.data
     }
   }
}
