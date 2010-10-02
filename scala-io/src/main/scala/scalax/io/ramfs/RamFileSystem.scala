/*                      __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,PathSet
}
import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.net.{
  URL,URI, URLStreamHandler
}
import java.util.UUID
import java.io.{IOException, FileNotFoundException}

object RamFileSystem {
  val protocol = "ramfs"
  private val fileSystems = scala.collection.mutable.WeakHashMap[String,RamFileSystem]()
  def apply(fsId:String) : RamFileSystem = synchronized {
    fileSystems.get(fsId).getOrElse(new RamFileSystem(fsId))
  }
  def apply(uri:URI) : RamPath = {
    require(uri.toString contains '!', "Ramfile system URIs must be of form: ramfs://fsId!path, was: "+uri+" (did not contain a !)")
    require(uri.getScheme equalsIgnoreCase "ramfs", "Ramfile system URIs must start with ramfs, was: "+uri)
    
    val id = uri.getAuthority.takeWhile{_ != '!'}
    val path = uri.getRawPath
    apply(id)(path)
  }
  private def register(fsId:String, fs:RamFileSystem) = synchronized {
    fileSystems(fsId) = fs
  }
}

class RamFileSystem(val id : String = UUID.randomUUID.toString) extends FileSystem {
  private var fsTree = new DirNode(separator)

  RamFileSystem.register(id,this)

  val root = new RamPath("",fsTree.name, this)
  var pwd = root

  val name = "Ram ("+id+")"
  override val urlStreamHandler : Option[URLStreamHandler] = Some(Handler)
  def separator: String = "/"
  def apply(path: String): RamPath = {
    if(path startsWith separator) apply("",path)
    else apply(pwd.toAbsolute.path, path)
  }
  def apply(relativeTo:String , path: String): RamPath = {
    def process(path:String) = {
//      println("regex",java.util.regex.Pattern.quote(separator))
      import java.util.regex.Pattern.quote
      val quotedSep = quote(separator)
      val p = path.replaceAll(quotedSep+"+", separator)
      if((p endsWith separator) && (p.length > 1)) p.drop(1)
      else p
    }
    new RamPath(process(relativeTo), process(path), this)
  } 
  def roots:List[RamPath] = List (root)
  def createTempFile(prefix: String = randomPrefix, 
                   suffix: String = null, 
                   dir: String = null,
                   deleteOnExit : Boolean = true
                   /*attributes:List[FileAttributes] TODO */ ) : Path = new RamPath("temp",UUID.randomUUID.toString,this)

  def createTempDirectory(prefix: String = randomPrefix,
                        suffix: String = null, 
                        dir: String = null,
                        deleteOnExit : Boolean = true
                        /*attributes:List[FileAttributes] TODO */) : Path  = new RamPath("temp",UUID.randomUUID.toString,this)
                        
  def uri(path:RamPath = root):URI = new URI(RamFileSystem.protocol+"://"+id+"!"+path.path)
  override def toString = "Ram File System"
  
  private[ramfs] def lookup(path:RamPath) = {
    val absolutePath = separator :: path.toAbsolute.segments
    fsTree.lookup(absolutePath)
  }
  private[ramfs] def create(path:RamPath, fac:NodeFac, createParents:Boolean = true) : Boolean = {
    val absolute = path.toAbsolute
    absolute.parent match {
      case Some(p) if p.notExists && !createParents => 
        throw new FileNotFoundException("Parent directory "+p+" does not exist")
      case _ => ()
    }

    val x = fsTree.create(absolute.segments,fac)
    
    true
  }
  private[ramfs] def delete(path:RamPath, force:Boolean) : Boolean = {
    if(path.exists) {
      def delete(p:Path) = force || (p.canWrite && p.parent.forall {_.canWrite})
    
      if(delete(path) && path != root) {
        val deletions = for { parent <- path.parent
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
