import xml.Node
import sbt.{Path,IO}
import java.io.File

class ProjectSite(val project:ProjectDescription) {
  val self = this;
  val name = project.name
  val summary = project.descSummary
  val description = project.description
  val path = project.projectPath
  def pagePath(page:ExamplesPage) = path+"/"+page.base+"/index.html"

  val pages = project.samplesSources map {path => new ExamplesPage(path,self)}


  def navbar(site:WebsiteModel, currPage:ExamplesPage, relativeToBase:String,currEx:Option[Example], showAllProjects:Boolean):Node = {
    <div id="navcontainer">
      <ul id="projectnavlist">
        <li><a href={relativeToBase+"/../index.html"}>Overview</a></li>
        <li><a href={relativeToBase+"/../getting-started.html"}>Getting Started</a></li>
        {for(project <- site.projectSites) yield {
        <li><a href={relativeToBase+project.name+"/index.html"}
               title={project.summary}
               class={if(project == self)"active" else ""}>{project.name.capitalize}</a>
            <a href={relativeToBase+"/"+project.name+"/scaladoc/index.html"}>(Scaladoc)</a>
          { if(project == self || showAllProjects) {
            <ul id="navlist">{for(page <- project.pages) yield {
                  if(currPage == page ){
                    <li>
                      <a title={page.uberSummaryText} class="active" href={relativeToBase+project.pagePath(page)}>{page.name}</a>
                      {page.pageNavList(currEx)}
                    </li>
                  } else {
                      <li><a title={page.uberSummaryText} href={relativeToBase+project.pagePath(page)}>{page.name}</a></li>
                  }
              }}</ul>
            } else {
              Nil
            }
          }
        </li>
      }}
        <li><a href={relativeToBase+"/../roadmap.html"}>Roadmap</a></li>
      </ul>
    </div>
  }


  def html(site:WebsiteModel) = {
    val content = <div><h3>Summary:</h3>{description}{ pages.map {
                    page =>
                      <div class="page">
                        <a href={"../"+pagePath(page)}>
                          <h3>{page.name}</h3>
                          <p class="page_summary">{page.summary}</p>
                        </a>
                        <div class="page_examples">
                        { page.examples.map {
                            ex =>
                              <div class="example_excerpt">
                                <a href={"../"+name+"/"+page.base+"/"+ex.htmlName}>
                                  <h3>{ex.name}</h3>
                                  <div class="example_excerpt_summary">{ex.summary}</div>
                                </a>
                              </div>
                        }}</div>
                      </div>
                  }}</div>
    Template(false,name.capitalize)(
      <link href="../css/samples.css" rel="stylesheet" type="text/css" ></link>)(
      <h1>{name.capitalize}</h1>
      <p class="summary">{summary}</p>)(
      content)(
      {navbar(site, null, "../",None,true)})

  }

  def buildSite(site:WebsiteModel) = {
    val projDir = new File(site.outputDir, path.getPath)
    projDir.mkdirs()
    Printer.printPage(new File(projDir, "index.html"),html(site))
    pages foreach{
      p =>
        val html = p.html(site)
        val pageDir = new File(projDir, p.base)
        Printer.printPage(new File(pageDir, "index.html"),html)
        p.examples foreach {
          ex =>
            val nav = navbar(site,p,"../../",Some(ex),false)
            val exHtml = Template.examplesTemplate(p,false,site,nav)(ex.html)
            Printer.printPage(new File(pageDir, ex.htmlName),exHtml)
        }
    }
  }
}
