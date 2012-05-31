package scalax.io

package object unmanaged {
    private[unmanaged] def unmanagedContext(base:ResourceContext) = base.copy(newByteBufferSize=Some((_,_) => 1), newCharBufferSize=Some((_,_) => 1))
}
