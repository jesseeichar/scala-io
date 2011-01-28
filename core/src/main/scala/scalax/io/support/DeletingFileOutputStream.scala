package scalax.io.support

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
import java.io.{
    File => JFile,
    FileOutputStream
}

class DeletingFileOutputStream(jfile:JFile, append : Boolean) extends FileOutputStream(jfile,append) {
    override def finalize() : Unit = {
        try      if(jfile.exists) jfile.delete()
        finally  super.finalize()
    }

    override def close() : Unit = {
      try super.close()
      finally if(jfile.exists) jfile.delete()
    }
}