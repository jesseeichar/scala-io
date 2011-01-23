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

  def examplesTemplate(page:ExamplesPage, showDesc:Boolean, site:WebsiteModel,navBar:NodeSeq)(exampleHtml:NodeSeq) = {
    val cssAndJs = (
      <script type="text/javascript" src="../../js/shCore.js"></script>
      <script type="text/javascript" src="../../js/shBrushScala.js"></script>
      <link href="../../css/shCore.css" rel="stylesheet" type="text/css"></link>
      <link href="../../css/shThemeDefault.css" rel="stylesheet" type="text/css" ></link>
      <link href="../../css/samples.css" rel="stylesheet" type="text/css" ></link>
    )
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
    Template(true,page.name)(cssAndJs)(header)(content)(navBar)
  }

}