
object SeekableSamples {

  { // examples of patching a file
    import scalax.io.{Codec, OverwriteAll,JavaConversions,Seekable}
    import JavaConversions.asResource
    import java.io.File

    // the codec must be defined either as a parameter of ops methods or as an implicit
    implicit val codec = scalax.io.Codec.UTF8

    val file: Seekable =  new File("file").asResource

    // write "people" at byte 6
    // if the file is < 6 bytes an underflow exception is thrown
    // if the patch extends past the end of the file then the file is extended
    file.patch(6, "people",OverwriteAll)
    file.patch(6, "people",OverwriteAll)(Codec.UTF8)

    // patch the file with a traversable of bytes
    file.patch(6, "people".getBytes,OverwriteAll)
  }

}
