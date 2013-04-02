import java.util.concurrent.TimeoutException

/**
 * Demonstrate how to perform asychronous IO code with scala-io.
 *
 */
object AsyncReadWrite {

  /**
   * Asychronously copy a section of one file to another.
   * <p>
   * The simplest method for performing asynchronous IO in scala-io is to
   * use the processor API to create a process and then turn it into a future.
   * (Or use the async method on LongTraversable, see other examples for that)
   * </p><p>
   *   The act of creating a Future will execute the process and return
   *   the result of the processor as the result of the future.
   * </p>
   */
  def basicProcessingFuture {
    import scalax.io.Resource
    import scala.util.{Success, Failure}

    val in = Resource.fromFile("/tmp/in")
    val out = Resource.fromFile("/tmp/out")

    // construct a processor in the normal way
    // make sure to yield something so that map
    // and flatmap calls are used instead of
    // foreach.
    val processor = for {
      in <- in.bytes.processor
      // drop the first 5 bytes
      _ <- in.drop(5)
      // create a Vector of the next 30 bytes
      data <- in.take(30)
      // write the data to the output
    } yield out.write(data)

    implicit val context = scalax.io.executionContext
    // execute the process
    // note that there is not repeat calls in the process
    // (like repeatUntilEmpty).  Thus the result is not
    // a LongTraversable.  if processor was of type
    // Processor[LongTraversable] futureExec would
    // have to be used instead.  See futureExec example
    // for more details.
    processor.future.onComplete {
      case Success(_) => println("Yay done :)")
      case Failure(_) => println("Uh oh failure :(")
    }

  }

  /**
   * Asynchronously read the header of a file and print the header in a more legible form.
   * <p>
   * The other simplest method for async IO is to create an AsyncLongTraversable from a LongTraversable
   * by calling the async method on LongTraversable.
   * </p><p>
   * Each LongTraversable object has a async method that provides access to the blocking
   * methods in a non-blocking manner. In other words, methods like foreach or apply will block the
   * calling thread until the data is read from the input source.  More often than not this is
   * not desired behaviour.
   * </p><p>
   * The async method on LongTraversable returns an object with a similar API to LongTraversable except
   * it only contains the blocking methods and those methods all return Futures.
   * </p><p>
   * Since the returned methods are futures the actual IO is performed asynchronously and the calling
   * thread can use the future to poll or register a callback for the result.
   * </p>
   */
  def asyncLongTraversableCallBack {
    import scalax.io.Resource

    val lines = Resource.fromFile("/tmp/file").lines()
    val HeaderPattern = """(???)(.*)""".r

    // once we have a LongTraversable we can convert the file to a AsyncLongTraversable and call
    // head on that object.  This will return a Future[String] and reading of the files will
    // have started in a seperate thread
    val headFuture = lines.async.head

    implicit val context = scalax.io.executionContext
    // When the data has been read from the file the result will be passed to the
    // function registered with onSuccess
    headFuture.onSuccess{
      case HeaderPattern(recordCount, description) => println("There are "+recordCount+": "+description)
    }
  }

  /**
   * Perform a copy of a file with timeouts the read and write actions and
   * execute the processor handling timeout errors.
   * <p>
   *   Processors can also have timeouts associated with each Processor
   *   which is useful both async and blocking modes.
   * </p>
   */
  def processorWithTimeOuts {
    import scalax.io.Resource
    import scala.concurrent.duration._

    val in = Resource.fromFile("/tmp/in")
    val out = Resource.fromFile("/tmp/out")

    // normal processor configuration
    // but add timeouts to certain key
    // processors that do the IO
    val processor = for {
      in <- in.blocks().processor
      outApi <- out.outputProcessor
      _ <- in.repeatUntilEmpty()
      block <- in.next.timeout(10 seconds)
      _ <- outApi.write(block).timeout(5 seconds)
    } yield ()

    // timeouts work on normal processors
    processor.onFailure {
      case e:TimeoutException =>
        println("Boo :( timout during processing")
        None
    }.execute()

    implicit val context = scalax.io.executionContext
    // timeouts work on normal processors
    processor.future.onFailure {
      case e:TimeoutException =>
        println("Boo :( timout during processing")
        None
    }
  }

  /**
   * Demonstrate Asynchronous data processing with processors that
   * result in a LongTraversable result.
   * <p>
   *   A common use of the processing API is to read an input as a
   *   LongTraversable of records.
   * </p>
   */
  def processorToLongTraversable {
    import scalax.io.Resource

    val chars = Resource.fromFile("/tmp/file").chars()
    val processor = for {
      api <- chars.processor
      _ <- api.repeatUntilEmpty()
      record <- api.take(5)
      _ <- api.drop(1)
    } yield record

    val processingTraversable = processor.traversable

    processingTraversable.async.mkString("\n")
  }

  /**
   * Several ways to use the Processor API to
   * copy data from one file to another.
   */
  def futureExec {
    import scalax.io.Resource
    import scala.util.{Success, Failure}

    val in = Resource.fromFile("/tmp/in")
    val out = Resource.fromFile("/tmp/out")

    // construct a processor in the normal way
    // make sure to yield something so that map
    // and flatmap calls are used instead of
    // foreach.
    val processor = for {
      in <- in.blocks().processor
      outApi <- out.outputProcessor
      // keep reading from in until there is no more data
      _ <- in.repeatUntilEmpty()
      // read the next block of data from in
      block <- in.next
      // write block to output
      _ <- outApi.write(block)
    // yield something so that the processor
    // is not executed as a for loop
    } yield ()

    implicit val context = scalax.io.executionContext
    // call future to execute the processor asynchonously
    // Note that because processor is a
    // LongTraversable (method repeatUntilEmpty is the cause of this)
    // future will only return the LongTraversable.  To
    // perform the writes we need the traversable to be traversed
    processor.futureExec.onComplete {
      case Success(_) => println("Yay done :)")
      case Failure(_) => println("Uh oh failure :(")
    }

    // A processor that performs the same task is as follows and
    // perhaps demonstrates better why futureExec is required a bit
    // more clearly

    val processor2 = for {
      in <- in.blocks().processor
      outApi <- out.outputProcessor
      _ <- in.repeatUntilEmpty()
      block <- in.next
    } yield outApi.asOutput.write(block)

    // in this example it is more clear that the write
    // is only performed as each element of the traversable
    // is visited.  Thus to do the entire write
    // the processor must be executed.
    processor2.futureExec()

    // A second way to perform the write (and in fact only a portion of the write)
    processor2.traversable.drop(5).take(5).async.foreach(_ => ()).onSuccess {
      case _ => println ("Yay, copied 5 blocks to the output")
    }
  }

  /**
   * Process only a portion of a file asynchronously.
   * A common case is to only process a portion of a file.  For example suppose we want the 5th record.
   * a record size is in the header so we have to read the header for the offset and jump to the 5th record.
   * <p>
   *   In truth this sort of processing is what the processing API was designed for but it can be done using
   *   the normal LongTraversable API as well using the fold methods
   * </p>
   */
  def asyncLongTraversableFolding {
    import scalax.io.{Resource, Continue, End}
    import scala.util.{Success, Failure}

    val bytes = Resource.fromFile("/tmp/file").bytes

    /*
     * The State classes define the different stages
     * of the process.  This has nothing to do with
     * the scala-io framework it is just a useful
     * way to implement folding in a readable way
     */
    trait State {
      def value:Seq[Byte] = Nil
    }

    /**
     * The first state.  If the state is Empty then the next element
     * is the record length which we need to record and skip to the
     * desired record with a Continue
     */
    case object Empty extends State

    /**
     * The second state.  Indicates we have the record length and
     * have jumped to the desired record.
     */
    case class Searcher(recordLength:Int) extends State

    /**
     * The final state.  Indicates that we are reading the record bytes
     * when recordLength == value.size we are done and can End the process
     */
    case class Value(recordLength:Int, override val value:Seq[Byte]) extends State

    // we could perform the fold on bytes but we will be smart and perform it
    // on the AsyncLongTraversable instead so that the process won't block
    val desiredRecord = bytes.async.limitFold(Empty:State) {
      case (Empty, recordLength) =>
        Continue(Searcher(recordLength), recordLength * 5)
      case (Searcher(recordLength),byte) =>
        Continue(Value(recordLength, Vector(byte)))
      case (Value(recordLength, sofar),next) if recordLength > sofar.size - 1 =>
        Continue(Value(recordLength, sofar :+ next))
      case (Value(recordLength, sofar),next) =>
        End(Value(recordLength, sofar :+ next))
    }

    implicit val context = scalax.io.executionContext
    // Last step is to handle the result from the process
    desiredRecord.onComplete {
      case Success(Value(_, record)) => () // do something
      case Failure(error) => () // uh oh need to handle error
    }
  }

  /**
   * Process a subsection of a file asychronously.
   */
  def longTraversableSlicingAndAsynchronisity {
    import scalax.io.Resource

    val lines = Resource.fromFile("/tmp/file").lines()

    // Many of the methods in LongTraversable are nonblocking
    // by nature.  All the mutation type function for example
    // because LongTraversable is a non-string collection.
    // IE it performs transformations as late as possible
    //
    // For example map, filter, zip, slice, etc... are all transforms that
    // have no effect until the data is requested at which point the data will
    // be transformed
    val linesOfInterest = lines.drop(10).take(4).filter(_ startsWith "A - ")

    // Execute foreach asynchronously.  The function will be executed
    // in a separate thread from the calling thread but the lines
    // will be passed to the function in the order they are read
    // from the input source
    linesOfInterest.async.foreach{line => println(line)}
  }
}
