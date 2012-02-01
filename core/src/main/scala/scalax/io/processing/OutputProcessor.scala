package scalax.io
package processing

import java.io.OutputStream
import java.io.FilterOutputStream

class OutputProcessor(resource:OutputResource[OutputStream]) extends Processor[OpenOutput] {
    def init = new Opened[OpenOutput]{
      val openedResource = resource.unmanaged
      def execute() = Some(new OpenOutput(openedResource))
      def cleanUp() = openedResource.close()
    }
}

class OpenOutput private[processing](out:OutputResource[OutputStream] with UnmanagedResource) extends Output {
    def underlyingOutput = out
}