package scalax.io.extractor

import java.io.FileInputStream
import java.io.OutputStream
import java.nio.channels.Channels
import java.nio.channels.WritableByteChannel

import scalax.io.Adapter

object WritableByteChannelExtractor {
	def unapply(obj:Any):Option[WritableByteChannel] = obj match {
	  case ad:Adapter[_] => unapply(ad.src)
	  case out:FileInputStream => Some(out.getChannel())
	  case out:OutputStream => Some(Channels.newChannel(out))
	  case chan:WritableByteChannel => Some(chan)
	  case _ => None
	}
}