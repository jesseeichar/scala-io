package scalax.io

import java.io.{FileOutputStream, PrintStream, BufferedOutputStream, File}
import sperformance.{CSVRunContext, PerformanceTest}

/**
 * This object is meant to be the final runner of the SPerformance test framework.
 */
object Main {
  var outputDirectory = new File("target/sperformance")


  def runTestsReflectively(tests: Class[_ <: PerformanceTest]*) {
    //TODO - ClassLoader magic....
    runTests(tests.map(_.newInstance) : _* )
  }

  def runTests(tests : PerformanceTest*) {
    for(test <- tests) {
      val context = new CSVRunContext(new File(outputDirectory,test.name+".csv"))
      test.runTest(context)
      context.writeResults()
    }
  }

  def main(args : Array[String]) {
    runTestsReflectively(args.map( arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]) : _*)
  }
}