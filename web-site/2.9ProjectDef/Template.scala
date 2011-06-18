import xml.{NodeSeq, Node}

object Template {
  def apply(hasCode:Boolean, title:String)
           (cssAndJs:Iterable[Node])
           (header:Iterable[Node])
           (content:Iterable[Node])
           (navbar:Iterable[Node]) = {

  <html>
    <head>
      <title>{title}</title>
      {cssAndJs}
    </head>
    <body>
      <div id="maincontainer">
        <div id="topsection">
          <div class="innertube">
            {header}
          </div>
        </div>

        <div id="contentwrapper">
          <div id="contentcolumn">
            <div class="innertube">
              {content}
            </div>
          </div>
        </div>

        <div id="leftcolumn">
          <div class="innertube">
            {navbar}
          </div>
        </div>

        <div id="footer">
          <div>Written by Jesse Eichar <a href="http://camptocamp.com">Camptocamp SA</a></div>
          <div>Twitter: <a href="http://twitter.com/jeichar">@jeichar</a></div>
          <div>(c) Jesse Eichar 2010-2011</div>
        </div>
      </div>
      {if(hasCode)
        <script type="text/javascript">
          SyntaxHighlighter.all()
        </script>
       else Nil
      }
    </body>
  </html>
  }
  def cssAndJs(relativeToBase:String) = (
      <script type="text/javascript" src={relativeToBase+"js/shCore.js"}></script>
      <script type="text/javascript" src={relativeToBase+"js/shBrushScala.js"}></script>
      <link href={relativeToBase+"css/shCore.css"} rel="stylesheet" type="text/css"></link>
      <link href={relativeToBase+"css/shThemeDefault.css"} rel="stylesheet" type="text/css" ></link>
      <link href={relativeToBase+"css/samples.css"} rel="stylesheet" type="text/css" ></link>
    )

  def examplesTemplate(page:ExamplesPage, showDesc:Boolean, site:WebsiteModel,navBar:NodeSeq)(exampleHtml:NodeSeq) = {

    val header =
      <span>
        <h1>{page.name}</h1>
        <p class="summary">{page.summary}</p>
      </span>
    val content =
      <span>
        {if (showDesc) page.description else Nil}
        {exampleHtml}
      </span>
    Template(true,page.name)(cssAndJs("../../"))(header)(content)(navBar)
  }

  def rootNavbar(active:Link,projectSites:List[ProjectSite]) = {
    import Link._
    <div id="navcontainer">
      <ul id="projectnavlist">
        <li><a href="./index.html" class={active.classAtt(Overview)}>Overview</a></li>
        <li><a href="getting-started.html" class={active.classAtt(GettingStarted)}>Getting Started</a></li>
        {for(project <- projectSites) yield {
        <li><a href={project.name+"/index.html"}
               title={project.summary}>{project.name.capitalize}</a> <a href={project.name+"/scaladoc/index.html"}>(Scaladoc)</a>
          <ul id="navlist">
            {for(page <- project.pages) yield {
              <li><a title={page.uberSummaryText} href={project.pagePath(page)}>{page.name}</a></li>
            }
          }</ul>
        </li>
      }}
        <li><a href="roadmap.html" class={active.classAtt(Roadmap)}>Roadmap</a></li>
      </ul>
    </div>
  }
}

trait Link {
  def classAtt(actual:Link) = if(this == actual) "active" else ""
}
object Link {
  case object Overview extends Link
  case object Roadmap extends Link
  case object GettingStarted extends Link
}