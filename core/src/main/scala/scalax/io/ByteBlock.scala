package scalax.io

trait ByteBlock {
  private[this] val self = this
  def apply(i: Int): Byte
  def size: Int
  def force:ByteBlock = new ByteBlock {
    val data = self.toSeq 
    override val force = this
    override val toSeq = data
    override val toIterator = data.toIterator
    val size = data.size
    def apply(i:Int) = data(i)
  }
  
  def toSeq = toIterator.foldLeft(Seq[Byte]())((acc,next) => acc :+ next)
  def toIterator = new Iterator[Byte] {
      private[this] val _size = self.size
      private[this] var pos = 0
      def next = {
        pos += 1
        self.apply(pos - 1)
      }
      def hasNext = 
        pos < _size
  }
}