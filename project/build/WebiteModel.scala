import sbt.{Logger, Path}
import xml.Node
import xsbt.FileUtilities._

object Printer {
  def printPage(to:Path,html:Node,log:Logger) = {
      log.info("Writing "+to.projectRelativePath)
      val htmlWithDocType: String =
        """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
         |"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
         |""".stripMargin+html
      write(to.asFile, htmlWithDocType, utf8)
    }
}

class ProjectSite(project:IoProject,log:Logger) {
  val self = this;
  val name = project.name
  val description = "description TODO"
  val path = project.info.projectPath.name
  def pagePath(page:ExamplesPage) = path+"/"+page.htmlName

  val pages = project.samplesSources.get map {path => new ExamplesPage(path,log,self)}

  def navbar(site:WebsiteModel, currPage:ExamplesPage) = {
    <div id="navcontainer">
      <ul id="projectnavlist">{for(project <- site.projectSites) yield {
        <li><a title={project.description} class={if(project == self)"active" else ""}>{project.name.capitalize}</a>
          { if(project == self) {
            <ul id="navlist">{for(page <- project.pages) yield {
                  if(currPage == page ){
                    <li>
                      <a title={page.uberSummaryText} class="active" href={"../"+project.pagePath(page)}>{page.name}</a>
                      {page.pageNavList}
                    </li>
                  } else {
                      <li><a title={page.uberSummaryText} href={"../"+project.pagePath(page)}>{page.name}</a></li>
                  }
              }}</ul>
            } else {
              Nil
            }
          }
        </li>
      }}</ul>
    </div>
  }


  def projectTemplate(project:ProjectSite) {

  }

  def buildSite(site:WebsiteModel) = {
    val projDir = site.outputDir / path
    projDir.asFile.mkdirs
    pages foreach{
      p =>
        val html = p.html(site)
        val pageDir = projDir / p.base
        Printer.printPage(pageDir / (p.htmlName+".html"),html,log)
        p.examples foreach {
          ex =>
            val exHtml = p.template(site)(ex.html)
            Printer.printPage(pageDir / (ex.htmlName),exHtml,log)
        }
    }
  }
}

case class Example(htmlName:String, name:String, summary:String, uberSummary:String, code:String) {
  override def toString = "Example("+name+", "+summary.substring(0,10)+"..., "+code.substring(0,10)+"...)"
  def html = {
    <div class="example">
      <a name={htmlName}><h3>{name}</h3></a>
      <p class="example_summary">{summary}</p>
      <div class="example_code">
        <pre class='brush: scala'>{code.replace("<","&lt;")}</pre>
      </div>
    </div>
  }
}

class WebsiteModel(val projectSites:List[ProjectSite],val outputDir:Path,log:Logger) {
  val self = this;
  def buildSite = {
    projectSites.foreach {
      project=>
        project.buildSite(self);
    }
  }

}
