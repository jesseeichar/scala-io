package scalax.io
package unmanaged

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    resource: A,
    resourceContext:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends ReadChars {

  self =>
  final val context = unmanagedContext(resourceContext)
  private[this] val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open, context)
}
