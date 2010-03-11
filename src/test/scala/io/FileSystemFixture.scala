/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import util.Random

import scalax.io._
import org.junit.rules.TemporaryFolder

object TestDataType extends Enumeration {
  type Type = Value
  val File, Dir = Value
}

case class TestData(fs : FileSystem, numSegments : Int, pathName : String) {
  val path = fs(pathName)
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
  import rnd.{nextInt}
  protected def rndInt(i:Int) = nextInt(i-1)+1
  val root = fs.createTempDirectory()

  def segment = fs.randomPrefix
    
  def file(segments:Int) : String = 1 to segments map {_ => segment} mkString (fs.separator)
  def file() : String = {
    val seg = rndInt(10)
    file(seg)
  }
  
  def path(segments : Int) = root / fs(file(segments))  
  def path = root / fs(file())
  
  def tree(depth : Int = rndInt(5)) = {
    for {d <- 0 until depth
         files <- 0 until rndInt(5) } {
           path(d).createFile(failIfExists  = false)
         }
    root
  }
  def testData(dataType : TestDataType.Type, create : Boolean = true) = {
    import TestDataType._
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
           data = fixture.testData(t, create)
           test(data)
        } catch {
          case e => 
            println(count+" tests passed before a failure case was encountered: \n"+data)
            throw e
        } finally {
          if(data!=null) try{data.delete()}catch{case _ =>}
        }
  }
  
  def after() : Unit = root.deleteRecursively()
}

class DefaultFileSystemFixture(val folder : TemporaryFolder, rnd : Random = new Random()) 
  extends FileSystemFixture(FileSystem.default, rnd) {
    folder.create()

    override val root = Path(folder.getRoot)
    override def after = folder.delete()
}