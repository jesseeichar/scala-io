package scalax.io.extractor

import java.io.InputStream
import scalax.io.Adapter
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel

object InputStreamExtractor {
	def unapply(obj:Any):Option[InputStream] = 
	  obj match {
	  case in:InputStream => Some(in)
	  case ad:Adapter[_] => unapply(ad.src)
	  case rc:ReadableByteChannel => Some(Channels.newInputStream(rc))
	  case _ => None
	}
}