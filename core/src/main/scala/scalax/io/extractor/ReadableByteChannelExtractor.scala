package scalax.io.extractor

import java.nio.channels.Channel
import java.io.FileInputStream
import java.nio.channels.Channels
import java.io.InputStream
import java.nio.channels.ReadableByteChannel
import scalax.io.ResourceAdapting.Adapter

object ReadableByteChannelExtractor {
	def unapply(obj:Any):Option[ReadableByteChannel] = obj match {
	  case ad:Adapter[_] => unapply(ad.src)
	  case in:FileInputStream => Some(in.getChannel())
	  case in:InputStream => Some(Channels.newChannel(in))
	  case chan:ReadableByteChannel => Some(chan)
	  case _ => None
	}
}