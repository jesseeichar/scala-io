/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import util.Random

import scalaio.test.{
  Constants, Node
}

import scalax.io._
import scalax.file._
import scalax.io.resource._

import java.io.InputStream

object TestDataType extends Enumeration {
  type Type = Value
  val File, Dir = Value
}

case class TestData(fs : FileSystem, numSegments : Int, pathName : String) {
  val path = fs.fromString(pathName)
  def delete() = path.parents.head.deleteRecursively()
  def create(dataType:TestDataType.Type) : TestData = {
    import TestDataType._
    dataType match {
      case File => path.createFile()
      case Dir => path.createDirectory()
    }
    this
  }
  lazy val access = Path.AccessModes.values filter {_ => Random.nextBoolean()} toSeq

  override def toString() = {
    "TestData( fs = %s, numSegments = %s, pathName = %s, exists = %s, access = %s)".format(fs, numSegments, pathName, path.exists, access)
  }
}

abstract class FileSystemFixture(val fs : FileSystem, rnd : Random) {
  import rnd.nextInt
  protected def rndInt(i:Int) = nextInt(i-1)+1
  val root = fs.createTempDirectory()

  def segment = fs.randomPrefix

  def file(segments:Int) : String = 1 to segments map {_ => segment} mkString (fs.separator)
  def file : String = {
    val seg = rndInt(10)
    file(seg)
  }

  def path(segments : Int, root : Path = root) : Path = root \ file(segments)
  def path : Path = root \ file

  /* Returns a Path and a Node.  Both the Node and the Path
   * have the same structure.  The nodes have the same names and subtree
   * as the equivalent path
   */
  def tree(depth : Int = rndInt(5)+2) : (Path, Node) = {

    val newRoot = path(1)
    newRoot.createDirectory(true)

    val structure = Node((newRoot.segments mkString Node.Sep).replaceAll(Node.Sep+"+",Node.Sep), None)
      for (d <- 1 until depth;
           files <- 0 until rndInt(5)) {
          val p = path(d, newRoot).createFile(failIfExists = false)

          p.relativize(root).segments.drop(1).foldLeft (structure){
            (parent, label) =>
              Node(parent.path + Node.Sep+ label, Some(parent))
          }
       }
    (newRoot, structure)
  }


  def testData(dataType : TestDataType.Type, create : Boolean = true) : TestData = {
    val seg = rndInt(10)
    val newFile = path(seg)
    val data = TestData(fs,newFile.segments.length,newFile.path)
    if(create) data.create(dataType)
    else data
  }

  def check(create:Boolean, test : TestData => Unit) : Unit = {
    var data : TestData = null
    for {count <- 1 to 50}
        try {
           val t = if(Random.nextBoolean) TestDataType.File else TestDataType.Dir
           data = testData(t, create)
           test(data)
        } catch {
          case e =>
//            println(count+" tests passed before a failure case was encountered: \n"+data)
            throw e
        } finally {
          if(data!=null) try{data.delete()}catch{case _ =>}
        }
  }
  /**
   * Copy resource from test resources to filesystem
   */
  def copyResource(source : InputStreamResource[InputStream]) : Path = {
      val dest = path
      dest write (source.bytes)
      dest
  }

  def copyResource(resource:String, base:Class[_]) : Path =
    copyResource(Resource.fromInputStream(Constants.resource(resource,base).openStream))

  /**
   * returns a path to a text file.  It is a copy of the file
   * in the test resources directory 'text'
   */
  def text(sep:String) = {
      val resource = Resource.fromInputStream {
          val bytes = Constants.TEXT_VALUE.replaceAll("""\n""", sep).getBytes(Codec.UTF8.name)
          new java.io.ByteArrayInputStream(bytes)
      }
      copyResource(resource)
  }

  /**
   * returns a path to a binary image file of size 6315.  It is a copy of the file
   * in the test resources directory 'image.png'
   */
  def image = copyResource(Resource.fromInputStream(Constants.IMAGE.openStream))

  def after() : Unit = root.deleteRecursively(force=true)
}
