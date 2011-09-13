package scalax.io.perf

import java.io.File

import sperformance.store.XmlLoadResults
import sperformance.store.XmlStoreResults
import sperformance.PerformanceTest

/**
 * This class is meant to be the final runner of the SPerformance test framework.
 */
object Main {
  var outputDirectory = new File("results")

  def runTestsReflectively(tests: Class[_ <: PerformanceTest]*) {
    //TODO - ClassLoader magic....
    runTests(tests.map(t => () => t.newInstance): _*)
  }

  def runTests(tests: (() => PerformanceTest)*) {
    for (testFactory <- tests) {
      val test = testFactory()
     println("Starting "+test.name)
      val context = new sperformance.HistoricalRunContext(outputDirectory, new XmlStoreResults(_), new XmlLoadResults(_))
      test.runTest(context)

      if (!"true".equalsIgnoreCase(System.getProperty("sperf.no.graphs"))) {
        context.generateResultsPage(test.name)
      }
    }
  }

  def main(args: Array[String]) {
    runTestsReflectively(args.map(arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]): _*)
  }
}