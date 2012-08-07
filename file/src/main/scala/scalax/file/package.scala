package scalax

package object file {
	type FileAttribute[A] = java.nio.file.attribute.FileAttribute[A]

	type FileAttributeView = java.nio.file.attribute.FileAttributeView
	type BasicFileAttributeView = java.nio.file.attribute.BasicFileAttributeView
	type DosFileAttributeView = java.nio.file.attribute.DosFileAttributeView
	type PosixFileAttributeView = java.nio.file.attribute.PosixFileAttributeView
	
	type BasicFileAttributes = java.nio.file.attribute.BasicFileAttributes
	type DosFileAttributes = java.nio.file.attribute.DosFileAttributes
	type PosixFileAttributes = java.nio.file.attribute.PosixFileAttributes
	
	
	type LinkOption = java.nio.file.LinkOption
	type CopyOption = java.nio.file.CopyOption
	
	def addAsFileAttributesToTupleSeq(tuple:Seq[(String,Any)]) = new {
	  def asFileAttributes = tuple.map(att => FileAttributeImpl(att._1, att._2))
	}
}