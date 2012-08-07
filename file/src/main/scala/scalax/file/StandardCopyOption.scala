package scalax.file

import java.nio.file.StandardCopyOption._

object StandardCopyOption {
	val replaceExisting = REPLACE_EXISTING
	val copyAttributes = COPY_ATTRIBUTES
	val atomicMove = ATOMIC_MOVE
}