package scalax.io
package processing

/**
 * ProcessorAPI for processing char input sources
 */
case class CharProcessor(base:CloseableIteratorProcessor[Char]) 
    extends SpecificApiFactory[Char, CharProcessorAPI](base) {
    protected def create(iter: CloseableIterator[Char]) = new CharProcessorAPI(iter, base.context)
}

class CharProcessorAPI private[processing](iter: CloseableIterator[Char],
                    resourceContext: ResourceContext) extends ProcessorAPI[Char](iter, resourceContext) {
  /**
   * Read the sequence of characters from the current element in the input source if A are Char.
   *
   * In practical terms the implicit portion of the method signature can be ignored.  It is required to make the method
   * type safe so that a method call to the method will only compile when the type of A is Char
   *
   * @param includeTerminator flag to indicate whether the terminator should be discarded or kept
   * @param lineTerminator the method to use for determine where the line ends
   * @param lineParser a case class to ensure this method can only be called when A are Chars
   *
   * @return a Processor containing the sequence of characters from the current element in the input source
   */
  def line(includeTerminator:Boolean = false, lineTerminator:Line.Terminators.Terminator = Line.Terminators.Auto) = {
      val wrapped = new CloseableIterator[Char]() {
        private[this] val proxy = iterator
        @inline final def next = proxy.next
        @inline final def hasNext = proxy.hasNext
        final def doClose = Nil
      }
      processFactory(Some(new LineTraversable(wrapped, lineTerminator, includeTerminator, resourceContext).headOption.getOrElse("").toSeq))
  }
  
}