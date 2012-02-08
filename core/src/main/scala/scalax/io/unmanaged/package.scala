package scalax.io

package object unmanaged {
    private[unmanaged] def unmanagedContext(base:ResourceContext) = base.copy(newByteBufferSize=Some(_ => 1), newCharBufferSize=Some(_ => 1))
}