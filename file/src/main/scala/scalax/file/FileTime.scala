package scalax.file

import java.nio.file.attribute.{FileTime => JFileTime}
import java.util.concurrent.TimeUnit
object FileTime {
  def fromMillis(time:Long): FileTime = FileTime(JFileTime.fromMillis(time))
  def apply(time: Long, unit:TimeUnit): FileTime = FileTime(JFileTime.from(time,unit))
}
case class FileTime(jfileTime: JFileTime) extends Ordered[FileTime] {
	def compare(other:FileTime) = jfileTime.compareTo(other.jfileTime)
	override def hashCode = jfileTime.hashCode
	override def equals(other: Any) = other match {
	  case other:FileTime => jfileTime == other.jfileTime
	  case _ => false
	}
	def to(unit: TimeUnit) = jfileTime.to(unit)
	def toMillis = jfileTime.toMillis
	override def toString = jfileTime.toString
}