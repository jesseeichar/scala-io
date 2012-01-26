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
 * } yield new Header(header1,header2)
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
class ProcessorAPI[+A](private[this] val iter: CloseableIterator[A]) {
  
  /**
   * Construct a sequence by taking elements from the input source until the function returns false or
   * there are no more elements in the input source.
   *
   * @param f the predicate that determines when to stop taking elements
   *
   * @return a Seq[A] consisting of the elements taken from the input source
   */
  def takeWhile(f: A => Boolean) = Processor(Some(bufferedIter takeWhile f))

  /**
   * Construct a sequence by taking up to i elements from the input source
   *
   * @param i the maximum number of elements to take
   *
   * @return a Seq[A] consisting of the elements taken from the input source
   */
  def take(i: Int) = Processor(Some(bufferedIter take i))

  /**
   * Drop/skip the next i elements in the input source if possible.
   *
   * Since dropping results in nothing the result can be ignored.
   *
   * {{{
   * // results in a Processor[Seq[<type of processor>]]
   * for {
   *   api <- input.processor
   *   _ <- api.drop(10)
   *   seq <- api.take(5)
   * } yield seq
   * }}}
   *
   * @param i the number of elements to skip
   * @return the returned Processor can typically be ignored since it is a unit processor.
   */
  def drop(i: Int) = Processor(Some(bufferedIter drop i))

  /**
   * Ends the ProcessAPI.  Any attempts to take or drop will have no effect after the process is ended.
   *
   * {{{
   * for {
   *   api <- input.processor
   *   _ <- repeatUntilEmpty
   *   seq <- api.take(5)
   *   if(seq contains 1)
   *   _ <- api.end
   * } yield seq
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
   * for {
   *   api <- input.processor
   *   _ <- repeatUntilEmpty()
   *   seq <- input.take(5)
   *   _ <- input.endIf(_ contains 25)
   * } yield seq
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
   *
   * The key differentiator between next and nextOption is that next in a for-comprehension will no yield a value where
   * nextOption will always contain a value, Some or None, so nextOption can be used if the element is optional to the process
   * but next will stop the process at that point.
   *
   * Example:
   * {{{
   * for {
   *   api1 <- input1.bytes.processor
   *   api2 <- input2.bytes.processor
   *   _ <- api1.repeatUntilEmpty()
   *   api1Next <- api.next
   *   next <- api2.next
   * } println(api1Next)
   *
   * for {
   *   api1 <- input1.bytes.processor
   *   api2 <- input2.bytes.processor
   *   _ <- api1.repeatUntilEmpty()
   *   api1Next <- api.next
   *   nextOption <- api2.nextOption
   * } println(api1Next)
   * }}}
   *
   * The two examples appear similar.  The first example will print the bytes from input1 until input2 is empty.  Where
   * as in example 2 will print all of api1 irregardless of whether api2 is empty or not.
   *
   * The reason is that in example 1 (next), next returns an empty processor when input2 is empty and thus the println
   * is not executed.  In example 2 the Processor is never empty it is either Some or None.
   * 
   * @note if the process has a repeatUntilEmpty() method call, nextOption should be preferred over next.  
   *       See repeatUntilEmpty for why
   *
   * @return An empty processor if there are no more elements in the input source or a processor containing the next element
   *        in the input source.
   */
  def next:Processor[A] = Processor(if(bufferedIter.hasNext) Some(bufferedIter.next) else None)

  /**
   * Read the next element in the input source and return Some(element) if there is a next element or None otherwise.
   *
   * See the documentation of next for details on how this method differs from next.
   *
   * @return a Processor containing Some(nextElement) if the input source is not empty or None if the input source if empty
   */
  def nextOption:Processor[Option[A]] = Processor(Some(if (bufferedIter.hasNext) Some(bufferedIter.next) else None))

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
  def line[B >: A](includeTerminator:Boolean = false, lineTerminator:Line.Terminators.Terminator = Line.Terminators.Auto)(implicit lineParser:LineParser[B]) =
    Processor(Some(lineParser.nextLine(includeTerminator, lineTerminator, bufferedIter)))
  /**
   * Create a Processor that simply repeats until this processor and all of the other input sources that are passed
   * in are empty or ended.  Each repetition generates an integer that can be used to count the number of repetitions
   * if desired. 
   * 
   * @note repeatUntilEmpty can very easily result in infinite loops because it depends on the following components
   * of the process/workflow correctly retrieving elements from the input source so that it eventually empties
   * 
   * The following examples are ways that one can create infinite loops (or loops that last up to Long.MaxValue):
   * 
   * {{{
   * for {
   *   processor1 <- input.bytes.processor
   *   processor2 <- input.bytes.processor
   *   processor1Loops <- processor1.repeatUntilEmpty()
   *     // if processor2 is emptied before processor1 there is an infinite loop because
   *     // this section will be the loop and since processor1 is not accessed here we have a loop
   *     // to be safer next1 should be in this section  
   *   processor2Loops <- processor2.repeatUntilEmpty()
   *   next1 <- processor1.nextOption
   *   next2 <- processor2.nextOption
   * } yield (next1, next2)
   * }}}
   * 
   * {{{
   * for {
   *   processor1 <- input.bytes.processor
   *   processor2 <- input.bytes.processor
   *   processor1Loops <- processor1.repeatUntilEmpty(processor2)
   *   next1 <- processor1.next  // nextOption should be used here because this can cause
   *                             // an infinite loop.  if processor1 is empty and processor2 is not
   *                             // next produces an empty processor so the next line will not be executed
   *                             // nextOption would always produce an non-empty Processor and therefore
   *                             // should be preferred over next
   *   next2 <- processor2.next
   * } yield (next1, next2)
   * }}}
   * 
   * {{{
   * for {
   *   processor1 <- input.bytes.processor
   *   processor2 <- input.bytes.processor
   *   loops <- processor1.repeatUntilEmpty(processor2)
   *   if loops < 100  // this guard is dangerous because if there are more than 100 elements in either
   *                   // processor1 or processor2 there is an infinite loop because next1 and next2 never get called
   *   next1 <- processor1.nextOption
   *   next2 <- processor2.nextOption
   * } yield (next1, next2)
   * }}}
   *
   * A safe implementation using repeatUntilEmpty should only execute methods that produce non-empty Processors
   * or should be done with extreme care.
   * 
   * for {
   *   processor1 <- input.bytes.processor
   *   processor2 <- input.bytes.processor
   *   processor1Loops <- processor1.repeatUntilEmpty(processor2)
   *   option1 <- processor1.nextOption
   *   option2 <- processor2.nextOption
   *   next1 <- option1
   *   next2 <- option2
   * } yield (next1, next2)
   * }}}

   * @param otherProcessorAPIs other processors to empty (in addition to this) before ending the loop
   * 
   * @return A Processor containing a sequence of whatever elements are returned by the for-comprehension
   */
  def repeatUntilEmpty(otherProcessorAPIs: ProcessorAPI[_]*) = new RepeatUntilEmpty(Long.MaxValue, (this +: otherProcessorAPIs): _*)
  
  /**
   * Loops n times or until the provided input sources are all empty.
   * 
   * This method is similar to repeatUntilEmpty except it limits the number of repetitions that can be performed.
   * 
   * @param times the maximum number of loops
   * @param otherProcessorAPIs other input sources to monitor for empty before prematurely ending the loop.  
   *                           If this and otherProcessorAPIs are all empty then the looping will be ended
   */
  def repeat(times: Int, otherProcessorAPIs: ProcessorAPI[_]*) = new RepeatUntilEmpty(times, (this +: otherProcessorAPIs): _*)

  // private methods follow
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
  private[this] def doEnd() = bufferedIter.end()
  private[this] var bufferedIter = new SpecializedBufferedIterator(iter)
  private[processing] val iterator = bufferedIter

}
