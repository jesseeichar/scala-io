/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.defaultfs

import scalax.io.{
  FileSystem, Path, FileOps,Codec,
  PathMatcher,DirectoryStream,OpenOption,
  Seekable, Process
}
import scalax.io.resource._
import scalax.io.OpenOption._
import scalax.resource.ManagedResource
import scalax.io.nio.SeekableFileChannel


import java.io.{ 
  FileInputStream, FileOutputStream, File => JFile, RandomAccessFile
}
import java.nio.channels.FileChannel
import java.net.{ URI, URL }

import collection.{Traversable }
import PartialFunction._
import util.Random.nextPrintableChar
import java.lang.{ProcessBuilder}

/**
 * <b>Not part of API.</b>
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
private[io] class DefaultFileOps(path : DefaultPath, jfile:JFile) extends FileOps(path) {

  def inputStream = Resource.fromInputStream(new FileInputStream(jfile))

  def outputStream(openOptions: OpenOption*) = {
      openOptions match {
          case Seq() => 
              openOutputStream(openOptions)
          case opts if opts forall {opt => opt != WRITE && opt != APPEND} => 
              openOutputStream(openOptions :+ WRITE)
          case _ =>
            openOutputStream(openOptions)
      }
  }

  def channel(openOptions: OpenOption*) = {
    val channel = new SeekableFileChannel(openChannel(openOptions))
    Resource fromByteChannel channel
  }
  def fileChannel(openOptions: OpenOption*) = Some(Resource fromByteChannel openChannel(openOptions))
  
  private def preOpen(openOptions: Seq[OpenOption]) : (Boolean, Seq[OpenOption]) = {
     val options = if(openOptions.isEmpty) OpenOption.WRITE_TRUNCATE
                    else openOptions

      var append = false
      options foreach {
          case APPEND => 
              append = true
          case CREATE if !jfile.exists =>
            jfile.createNewFile()
          case CREATE_FULL if !jfile.exists =>
            jfile.getParentFile.mkdirs()
            jfile.createNewFile()
          case CREATE_NEW =>
            if (jfile.exists) Path.fail(jfile+" already exists, openOption "+CREATE_NEW+" cannot be used with an existing file")
          case TRUNCATE if (openOptions contains WRITE) && (jfile.length > 0)=>
            new FileOutputStream(jfile).close()  // truncate file
            
          case _ => ()
      }
      
      (append,options)
  }
  
  private def openOutputStream(openOptions: Seq[OpenOption]) = {
    val (append, options) = preOpen(openOptions)
    if (options contains DELETE_ON_CLOSE) {
        Resource fromOutputStream new DeletingFileOutputStream(jfile, append)
    } else {
        Resource fromOutputStream new FileOutputStream(jfile,append)
    }
  }

  private def openChannel(openOptions: Seq[OpenOption]) = {
    val (_, options) = preOpen(openOptions)
    if (options contains DELETE_ON_CLOSE) {
       throw new UnsupportedOperationException("DELETE_ON_CLOSE is not supported on FileChannels pre Java 7 implementations.")
    } else {
        randomAccessFile(options)
    }
  }

  private def randomAccessFile(openOptions: Seq[OpenOption]) = {
      val unsortedChars = openOptions collect {
          case WRITE | APPEND => 'w'
          case READ => 'r'
          case SYNC => 's'
          case DSYNC => 'd'
      }
      
      
      // only values acceptable to RandomAccessFile are r rw rwd or rws
      // so need to do some massaging
      val sortedChars = unsortedChars.distinct.sortWith{ (x,y) => 
          def value(x : Char) = x match {
              case 'r' => 0
              case 'w' => 1
              case 's' => 2
              case 'd' => 3
          }
          value(x) < value(y)
      }
      
      val chars = if(sortedChars.mkString endsWith "sd") sortedChars.takeWhile(_ != 's') 
                  else sortedChars

      val file = if(chars contains 'r') {
          new RandomAccessFile(jfile, chars mkString)
      } else {
          new RandomAccessFile(jfile, 'r' + chars.mkString)
      }

      if(openOptions contains APPEND) file.seek(file.length)

      file.getChannel
  }
  

  def open[R](openOptions: Seq[OpenOption] = List(WRITE))(action: Seekable => R): R =  {
      null.asInstanceOf[R]
  }

  def withLock[R](start: Long = 0, size: Long = -1, shared: Boolean = false)(block: => R): Option[R] = {
    None
  }
    
  def execute(args:String*)(implicit configuration:ProcessBuilder=>Unit = p =>()):Option[Process] = {
    import Path.fail
    
    if(!jfile.exists) fail(jfile+" can not be executed as it does not exist")
    if(!jfile.canExecute) fail(jfile+" can not be executed as the execution access option is not set")
    
    null // TODO
  }

  protected def seekableChannel(openOptions:OpenOption*) = fileChannel(openOptions:_*).get
}
