package scalax.io.resource

import java.io.{
    File => JFile,
    FileOutputStream
}

protected[io] class DeletingFileOutputStream(jfile:JFile, append : Boolean) extends FileOutputStream(jfile,append) {
    override def finalize() : Unit = {
        try      if(jfile.exists) jfile.delete()
        finally  super.finalize()
    }
    
    override def close() : Unit = {
        try      if(jfile.exists) jfile.delete()
        finally  super.close()
    }
}