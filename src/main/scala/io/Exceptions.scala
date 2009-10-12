package scalax.io

case class FileOperationException(msg: String) extends RuntimeException(msg)
case class FileAlreadyExistsException(msg: String) extends FileOperationException(msg)