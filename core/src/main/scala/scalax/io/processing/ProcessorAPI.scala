package scalax.io
package processing

/**
 * The Main API for declaring a process workflow on an Input object.  This class contains all the operations
 * that can performed.  A ProcessorAPI object essentially wraps a data source/resource that provides
 * elements of type A, the methods allow the developer to read and skip over the elements as needed.
 *
 * The processing package contains classes for processing and the ProcessorAPI is the core of 
 * the Processing API.
 *
 * The concept behind the processing API is to declare the format in a largely declarative manner and read the contents
 * as needed.  The processing API is largely not needed when only a single input object is to be processed however
 * it could be considered to be slightly easier to read but when multiple input objects are to be processed at the same
 * time and have interdependencies then the processing API becomes indispensable.
 *
 * There are two commonly used patterns: extract a single item of data from Input objects and extract a collection of
 * data elements from Input objects.
 *
 * Pattern 1: Read a single item of data from Input objects:
 *
 * {{{
 * val headerProcessor = for {
 *   api1 <- input.bytes.processor
 *   api2 <- input2.bytes.processor
 *   header1 <- api.take(100)
 *   header2 <- api2.take(100)
 *  } yield new Header(header1,header2)
 *
 * headerProcessor.acquireAndGet(header => /* do something with header */)
 * }}}
 *
 * In this use case the format is declared as the for-comprehension and the result is returned by the yield.
 * The val headerProcessor is not executed executed until acquireAndGet is executed, at that point it essentially
 * is the process workflow for reading the headers.  The acquireAndGet method executes the process and passes the resulting
 * object to the function argument.
 *
 * @note There is no method for directly obtaining the result of the
 * process because of the next use-case.  As you will see the ProcessorAPI has repeat*** methods that result in
 * Processor[Iterator[A] ], in this case the iterator only works when the resources are open.  Because of this all processors
 * must be accessed within a context where they will be correctly closed after accessing.
 *
 * In short, accessing the result of a Processor is done within the acquireAndGet method (or via traversable, see next section).
 *
 * Pattern 2: Define a repetitive format and obtain a LongTraversable[A] for processing each element defined by the format:
 *
 * {{{
 * val process = for {
 *   api1 <- input.bytes.processor
 *   api2 <- input.chars.processor
 *   _ <- api1.repeatUntilEmpty()
 *   binaryData <- api1.take(100)
 *   attributes <- api2.takeWhile(_ != '\n')
 *  } yield new Record(binaryData,attributes)
 *
 * process.traversable // do something with traversable of records
 * }}}
 *
 * In this example the entire api1 is processed until it is empty.  The value ''process'' is of type Process[Iterator[Record] ]
 * and can be accessed by using either acquireAndGet or traversable.  acquireAndGet behaves the same way as for Pattern 1
 * and the iterator is accessed as normal.  However the iterator must not escape the acquireAndGet because it will
 * no longer be valid because the resources it depends on will be closed, thus the traversable is often a better option because
 * it returns a LongTraversable and can be used repeatedly without concern about the underlying resources being left open or
 * closed while the traversable continues to be used.
 *
 * @note when using repeatUntilEmpty() the following lines must consume data from the ProcessorAPI otherwise the loop will
 * never terminate (because it will never be empty)
 *
 * @note repeatUntilEmpty takes parameters so that it will repeat until a set of ProcessorAPI objects are empty rather just one
 *
 * Notice the difference between:
 *
 * {{{
 * for {
 *   api1 <- input.bytes.processor
 *   api2 <- input.chars.processor
 *   _ <- api1.repeatUntilEmpty()
 *   _ <- api2.repeatUntilEmpty()
 *   ...
 * } ...
 * }}}
 *
 * and
 *
 * {{{
 * for {
 *   api1 <- input.bytes.processor
 *   api2 <- input.chars.processor
 *   _ <- api1.repeatUntilEmpty(api2)
 *   ...
 * } ....
 * }}}
 *
 * and
 *
 * {{{
 * for {
 *   api1 <- input.bytes.processor
 *   _ <- api1.repeatUntilEmpty()
 *   api2 <- input.chars.processor
 *   _ <- api2.repeatUntilEmpty()
 *   ...
 * } ....
 * }}}
 *
 * In case 1 the process follow api2.repeatUntilEmpty will be repeated until api2 is empty and if api1 is not empty the
 * process will never end.  This is because api1 will repeatUntilEmpty but api2 is empty so it will simply return
 *
 * In case 2 the process will repeat until both api1 and api2 are empty
 *
 * In case 3 api2 will be created then the process will repeat until api2 is empty.  Then api1 will be checked to see if
 * it is empty.  If it is then the api2 will be created again and the process will be repeated until api2 is again empty. etc...
 *
 * All the normal behaviour of for-comprehensions are supported as well, including guards, pattern matching etc...
 */
class ProcessorAPI[+A](private[this] var iter: CloseableIterator[A]) {
  /**
   * Construct a sequence by taking elements from the input source until the function returns false or
   * there are no more elements in the input source.
   *
   * @param f the predicate that determines when to stop taking elements
   *
   * @return a Seq[A] consisting of the elements taken from the input source
   */
  def takeWhile(f: A => Boolean) = createSeq[A](iter takeWhile f)

  /**
   * Construct a sequence by taking up to i elements from the input source
   *
   * @param i the maximum number of elements to take
   *
   * @return a Seq[A] consisting of the elements taken from the input source
   */
  def take(i: Int) = createSeq[A](iter take i)

  /**
   * Drop/skip the next i elements in the input source if possible.
   *
   * Since dropping results in nothing the result can be ignored.
   *
   * {{{
   *     // results in a Processor[Seq[<type of processor>]]
   *     for {
   *       api <- input.processor
   *       _ <- api.drop(10)
   *       seq <- api.take(5)
   *     } yield seq
   * }}}
   *
   * @param i the number of elements to skip
   * @return the returned Processor can typically be ignored since it is a unit processor.
   */
  def drop(i: Int) = createSideEffect(updateIter(ops.drop(i)))

  /**
   * Ends the ProcessAPI.  Any attempts to take or drop will have no effect after the process is ended.
   *
   * {{{
   *   for {
   *     api <- input.processor
   *     _ <- repeatUntilEmpty
   *     seq <- api.take(5)
   *     if(seq contains 1)
   *     _ <- api.end
   *   } yield seq
   * }}}
   *
   * Example takes 5 elements from the input source until it contains the value 1 then it ends the repeating and returns
   * the seq.
   *
   * The resulting Processor will be of type Processor[Iterator[Seq[A] ] ], even though there can only be one element, thus
   * calling: result.traversable.headOption is likely the easiest method of obtaining the value from the Processor
   *
   * Just a side note:
   *   This could be done with a LongTraversable with:  traversable.sliding(5,5).filter(_ contains 1).headOption
   *   Note that you need to use headOption because you don't know if there are any elements in the resulting traversable,
   *   however the LongTraversable API would be difficult to use if you want to processes multiple input objects together
   *
   * @return a Processor[Unit] and therefore the result can normally be ignored
   */
  def end() = createSideEffect(doEnd())

  /**
   * End the processes if the predicate return true.  Any attempts to take or drop will have no effect after the process is ended.
   *
   * {{{
   *     for {
   *       api <- input.processor
   *       _ <- repeatUntilEmpty()
   *       seq <- input.take(5)
   *       _ <- input.endIf(_ contains 25)
   *    } yield seq
   * }}}
   *
   * The Example takes 5 elements of the input until one of the sequences contains the number 25.
   *
   * As a side note:
   *   This could be done using the LongTraversable API: traversable.sliding(5,5).takeWhile(i => !(i contains 25))
   *   While this example can be done with the normal traversable API, this API is typically preferred when
   *   reading data from multiple interdependent sources.
   *
   * @param f the predi
   * @return a Processor[Unit] and therefore the result can normally be ignored
   */
  def endIf(f: => Boolean) = createSideEffect(if(f) doEnd())

  /**
   * Read the next element in the input source if possible.
   *
   * If there is an element left in the input source a Processor containing that element will be returned, otherwise
   * the returned processor will be empty.
   */
  def next:Processor[A] = Processor(if(iter.hasNext) Some(iter.next) else None)
  def nextOption:Processor[Option[A]] = Processor(Some(if (iter.hasNext) Some(iter.next) else None))
  def repeatUntilEmpty(otherProcessorAPIs: ProcessorAPI[_]*) = RepeatUntilEmpty((this +: otherProcessorAPIs): _*)
  def repeat(times: Int) = Repeat(times)

  // private methods follow
  private[this] var ops = CloseableIteratorOps(iter)
  private[this] def updateIter(f: => CloseableIterator[A]) = {
    iter = f
    ops = CloseableIteratorOps(iter)
  }
  private[processing] def iterator = iter
  private[this] def createSideEffect(f: => Unit) = new Processor[Unit] {
    private[processing] def init = new Opened[Unit] {
      def cleanUp() = ()
      def execute() = Some(f)
    }
  }

  private[this] def createSeq[U](f: => Iterator[U]) = Processor({
    val builder = new collection.immutable.VectorBuilder[U]()
    builder ++= f
    Some(builder.result())
  })
  private[this] def doEnd() = iter = iter.take(0)

}
