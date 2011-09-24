import sbt.IO
import java.io._
import java.nio.charset.Charset

object PerformanceReport {
  val utf8 = Charset.forName("UTF-8")
  def buildSite(outDir:File, inDir:File):Seq[Keyword] = {
	assert(inDir.listFiles != null, inDir+" does not have any performance reports")
    val reportDirs = inDir.listFiles.filter(_.isDirectory)
    reportDirs.zipWithIndex flatMap { case (reportDir,i) =>
      val name = {
        val raw = reportDir.getName.split("\\.").last
        if(raw endsWith "Test") {
          raw.dropRight(4)
        } else {
          raw
        }
      }
      val children = reportDir.listFiles
      val childNames = children.map(childName)
      val keyword = Keyword("performance",name,formatName(name)+" Performance Report",1,formatName(name),"performance",Set("performance", name.toLowerCase) ++ childNames.map(_.toLowerCase))
      val childKeywords = childNames.map{n => Keyword("performance",childId(reportDir,n),n.capitalize,2,n.capitalize,"performance graph",Set("performance", n.toLowerCase))} 
      
      IO.write(new File(outDir, name+".html"), mainReport(reportDir,name,children).toString, utf8)
      IO.copyDirectory(inDir, outDir)
      children.foreach { child => 
        val htmlFile = new File(outDir, childId(reportDir, childName(child))+".html")
        IO.write(htmlFile,childReport(child).toString, utf8)
      }
      keyword +: childKeywords
    }
   
  }
   
  def mainReport(reportDir:File,name:String,children:Seq[File]) = {
    val columns = 2
    val namesInRows = children.sliding(columns,columns)
    <span id="performance">Performance reports for module {name.capitalize}
      <table>
        {for(r <- namesInRows) yield {
              <tr>{for(c <- r) yield 
                <td>
                  <a href={"{{getUrl({section:'performance',id:'"+childId(reportDir,childName(c))+"'})}}"}>
                    <img class="report" src={"performance/"+reportDir.getName+"/"+c.getName}></img>
                  </a>
                </td>}</tr> 
            }
        }
      </table>
    </span>
  }
  def formatName(string:String)=string.capitalize.flatMap{
    case c if c.isUpperCase => " "+c
    case c => Seq(c)
  }.trim
  def childReport(imgFile:File) = {
    <img id="performanceGraph" src={"performance/"+imgFile.getParentFile.getName+"/"+imgFile.getName}></img>
  }
  def childName(imgFile:File) = imgFile.getName.dropRight(".png".size)
  def childId(reportDir:File, name:String) = (reportDir.getName+"_"+name).replace(" |/|\\\\","_")
}