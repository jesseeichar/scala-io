package scalax.io.extractor

import scalax.io.Adapter
import java.nio.channels.FileChannel
import java.io.FileInputStream

object FileChannelExtractor {
  def unapply(obj:Any):Option[FileChannel] =
    obj match {
    case fin:FileInputStream => Some(fin.getChannel)
    case fc:FileChannel => Some(fc)
    case ad:Adapter[_] => unapply(ad.src)
    case _ => None
  }
}
