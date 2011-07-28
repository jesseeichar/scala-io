package scalax.io

import java.io.File

import sperformance.store.XmlLoadResults
import sperformance.store.XmlStoreResults
import sperformance.PerformanceTest

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
    val context = new sperformance.HistoricalRunContext(outputDirectory, new XmlStoreResults(_), new XmlLoadResults(_))
    test.runTest(context)

    context.generateResultsPage(test.name)
    }
  }

  def main(args : Array[String]) {
    runTestsReflectively(args.map( arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]) : _*)
  }
}