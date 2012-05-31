package scalax.io.perf
import java.io.OutputStream

object NullOutputStream extends OutputStream {
  override def close:Unit = ()
  override def flush:Unit = ()
  override def write(b:Array[Byte]) = ()
  override def write(b:Array[Byte],off:Int, len:Int) = ()
  override def write(b:Int) = ()
}
