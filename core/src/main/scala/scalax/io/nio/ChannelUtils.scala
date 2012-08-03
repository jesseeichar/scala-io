package scalax.io.nio

import java.nio.channels.SeekableByteChannel

object ChannelUtils {
  def read(channel: SeekableByteChannel, dst : java.nio.ByteBuffer, pos:Long) : Int = doAt (channel, pos){channel.read(dst)}
  def write(channel: SeekableByteChannel, buffer: java.nio.ByteBuffer, pos: Long) = doAt(channel, pos){channel.write(buffer)}
  private def doAt[T](channel: SeekableByteChannel, pos:Long)(f: => T) = {
    val mark = channel.position
    channel.position(pos)
    val result = f
    channel.position(mark)
    result
  }
}