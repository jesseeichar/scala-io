/**
 * Shows how to obtain a java io File from a scala io Path (when possible).
 */
object PathsToJavaFile {
	/**
	 * Not all Paths can be converted to java.io.File objects because a Path
	 * is not necessarily a file on the default file system.  It could be a file
	 * in a zip file.  As such one must first check to see if the the Path is
	 * a DefaultPath.  Once that is established the jfile can be obtained from
	 * the DefaultPath.
	 * <p>
	 * In java 7 scala io paths will be able to be converted to java 7 paths
	 * </p>
	 */
  def pathToJavaFile {
    import scalax.file.Path
    import scalax.file.defaultfs.DefaultPath
    import java.io.File 
    
    val somePath = Path("/somedir/somefile.txt")
    somePath match {
      case defaultPath:DefaultPath => 
        val file:File = defaultPath.jfile
        // do something with file
      case  _ => 
        // handle non-file case
    }
  }
}