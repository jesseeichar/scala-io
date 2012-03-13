package scalax.io
package processing;

/**
 * A point or step in a IO process workflow.
 *
 * The processing API is declarative way to process several input streams together.  The allows much more powerful
 * options beyond what is available using the normal collections-like API available in [[scalax.io.LongTraversable]].
 *
 * {{{
 * val firstElems = for {
 *   processor <- longTraversable.processor
 *   firstElem <- processor.next
 *   secondElem <- processor.next
 * } yield (firstElem, secondElem)
 *
 * firstElems.acquireAndGet(println)
 * }}}
 *
 * Ignore for the moment that the above example is trivial.
 *
 * The for-comprehension defines the process.  But nothing is executed until the result (firstElems) is accessed with
 * acquireAndGet.  This is because IO typically accesses resources which must be opened and closed.  When acquireAndGet
 * is executed. The Processor[ProcessorAPI[A]] obtained from the longTraversable is opened and the first and second element
 * are read from the processor (via the ProcessorAPI which defines the operations permitted for this process pipeline).
 * The result is passed to the function that was initially passed to acquireAndGet.  After the function completes the process is closed.
 *
 * For more details on how the processing API is used look at the [[scalax.io.processing.ProcessorAPI]] documentation.
 *
 * It is possible for a Processor to be empty so acquireAndGet returns an Option to support this case.  Consider:
 *
 * {{{
 * import scalax.io.JavaConverters._
 * val result = for {
 *   p <- List(1,2,3).asInput.bytes.processor
 *   next <- next
 *   if false
 * } yield next
 *
 * result.acquireAndGet(println)
 * }}}
 *
 * Obviously the if false means the Processor will not contain any values so the println will not be executed and
 * None will be retured from acquireAndGet
 *
 * @see [[scalax.io.processing.ProcessorAPI]]
 *
 * @tparam A the type of the object that will be the result of running this Processor
 */
trait Processor[+A] {
  self =>

  protected[processing] def context: ResourceContext

  protected def processFactory = new ProcessorFactory(context)
  /**
   * Convert the Processor into a LongTraversable if A is a subclass of Iterator.
   */
  @scala.annotation.implicitNotFound("The Processor type (A) is not a subclass of Iterator[A] and thus cannot be used to create a LongTraversable.  Check that you have a .repeat___ method in the process pipeline")
  def traversable[B](implicit transformer: ProcessorTransformer[B, A, LongTraversable[B]]): LongTraversable[B] = transformer.transform(this)

  /**
   * Execute the process workflow represented by this Processor and pass the function the result, if the Processor
   * is nonEmpty.
   *
   * @note If A is an iterator do not return it since it might not function outside the scope of acquireAndGet.
   *
   * @return the result of the function within a Some if this processor is Non-empty.  Otherwise the function
   * will not be executed and None will be returned
   */
  def acquireAndGet[U](f: A => U): Option[U] = {
    val initialized = init
    try initialized.execute.map(f)
    finally initialized.cleanUp
  }

  def onError[U >: A](handler:PartialFunction[Throwable,Option[U]]):Processor[U] = new Processor[U] {
    protected[processing] def context = self.context

    private[processing] def init = new Opened[U] {
      private[this] val outer = self.init
      def execute = try outer.execute
                    catch handler
      def cleanUp() = outer.cleanUp
    }
  }

  /**
   * Execute the Processor.  If the result is an iterator then execute() will visit each element
   * in the iterator to ensure that any processes mapped to that iterator will be executed.
   */
  def execute():Unit = {
    def runEmpty(e:Any):Unit = e match {
      case lt:LongTraversable[_] => lt.foreach(runEmpty)
      case _ => ()
    }
    val initialized = init
    try {
      initialized.execute match {
        case Some(iter:LongTraversable[_]) =>
          iter.foreach(runEmpty)
        case result =>
          ()
      }
    }finally initialized.cleanUp
  }
  /**
   * Convert this Processor to a Processor containing an Option.  Methods such as next return a potentially empty Processor which will,
   * when in a for comprehension, will stop the process at that point.  Converting the processor to an option allows the process handle
   * continue and simply handle the possibility of one input source being empty while other continue to provide data.
   *
   * Consider the following example:
   * {{{
   * for {
   *   idsIn <- ids.bytesAsInts.processor
   *   attributes <- in.lines().processor
   *   _ <- idsIn.repeatUntilEmpty(attributes)
   *   id <- ids.next.opt.orElse(NoId)
   *   attr <- attributes.next.opt.orElse("")
   * } yield new Record(id,attr)
   * }}}
   *
   * The above example processes the streams completely even if one ends prematurely.
   */
  def opt = new Processor[Option[A]] {
    def context = self.context

    private[processing] def init = new Opened[Option[A]] {
      private[this] val outer = self.init
      def execute = Some(outer.execute)
      def cleanUp() = outer.cleanUp
    }
  }

  /* opens the resource if necessary and allows the processor to be executed */
  private[processing] def init: Opened[A]

  /**
   * Apply a filter to this processor.  If the filter returns false then the resulting Processor will be empty.  It is
   * not possible to know if the Processor is empty unless acquireAndGet is called because the filter is not called until
   * acquireOrGet is executed (or the Processor is somehow processed in another way like obtaining the LongTraversable
   * and traversing that object).
   *
   * @return A new Processor with the filter applied.
   */
  def filter(f:A => Boolean) = new WithFilter(this,f)

  /**
   * Same behavior as for filter.
   */
  def withFilter(f:A => Boolean) = new WithFilter(this,f)

  /**
   * Execute the Processor and pass the result to the function, much like acquireAndGet but does not return a result
   */
  def foreach[U](f: A => U): Unit = acquireAndGet(f)
  /**
   * Map the contents of this Processor to a new Processor with a new value.
   *
   * The main use case is so Processor work in for-comprehensions but another useful use case is
   * to convert the value read from a ProcessorAPI to a new value.  Suppose the value read was an integer you might
   * use map to convert the contained value to a float.
   */
  def map[U](f: A => U) = processFactory[Opened[A], U](init, in => in.execute.map(f), _.cleanUp)
  def flatMap[U](f: A => Processor[U]) = {
    processFactory[(Opened[A], Option[Opened[U]]), U]({
      val currentOpenProcessor = self.init
      (currentOpenProcessor, currentOpenProcessor.execute.map(f(_).init))
    },
      _._2.flatMap(_.execute),
      in => { in._2.map(_.cleanUp); in._1.cleanUp() })
  }
}

/**
 * Represents an open process.  IE the process has been opened and is in the middle of an operation.
 * The cleanUp method must be called when the operation is done.
 *
 * This class is only for private implementation purposes
 */
private[processing] trait Opened[+A] {
  def execute: Option[A]
  def cleanUp(): List[Throwable]
}

/**
 * Factory methods for creating Processor objects
 */
class ProcessorFactory(val resourceContext: ResourceContext) {
  /**
   * Create a new Processor
   *
   * @param opener the function that opens the resource
   * @param valueFactory a function that creates the value from the opened resource
   * @param after method that cleans up the resource
   */
  def apply[B, A](opener: => B, valueFactory: B => Option[A], after: B => List[Throwable]) = new Processor[A] {
    def context = resourceContext
    private[processing] def init = {
      val resource = opener
      new Opened[A] {
        def execute() = valueFactory(resource)
        def cleanUp() = after(resource)
      }
    }
  }

  /**
   * Create a stateless Processor
   *
   * @param valueFactory a function that creates the value
   */
  def apply[A](valueFactory: => Option[A]) = new Processor[A] {
    def context = resourceContext
    private[processing] def init = {
      new Opened[A] {
        def execute() = valueFactory
        def cleanUp() = Nil
      }
    }
  }

  /**
   * the empty processor
   */
  def empty[A] = new Processor[Nothing] {
    def context = resourceContext
    private[processing] def init = null.asInstanceOf[Opened[Nothing]]
    override def foreach[U](f: Nothing => U): Unit = ()
    override def map[U](f: Nothing => U) = this
    override def flatMap[U](f: Nothing => Processor[U]) = this
  }
}

/**
 * An internal implementation Processor containing a ProcessorAPI.
 */
private[io] class CloseableIteratorProcessor[+A](private[processing] val iter: () => CloseableIterator[A], resourceContext: ResourceContext) extends Processor[ProcessorAPI[A]] {
  def context = resourceContext
  private[processing] def init = {
    val iterInstance = iter()
    val processorAPI = new ProcessorAPI[A](iterInstance, resourceContext)
    new Opened[ProcessorAPI[A]] {
      def execute() = Some(processorAPI)
      def cleanUp() = iterInstance.close()
    }
  }
}

/**
 * A Processor that applies a filter to another Processor and thus is either an empty Processor or a normal processor
 * depending on the result of applying the filter.  There is no way to detect if the result is empty or not until
 * the Processor is executed.
 */
private[processing] class WithFilter[+A](base:Processor[A], filter: A=>Boolean) extends Processor[A] {
    private[processing] def init = base.init
    def context = base.context

    override def foreach[U](f: A => U): Unit =
      base.init.execute.filter(filter).foreach(f)
    override def map[U](f: A => U) =
      processFactory(base.init.execute.filter(filter).map(f))
    override def flatMap[U](f: A => Processor[U]) =
      processFactory(base.init.execute.filter(filter).flatMap(f(_).init.execute))
}