object GettingStarted {

  def html(model:WebsiteModel) = {
    val version = model.rootProject.version.toString
    val groupId = model.rootProject.organization
    val armVersion = model.rootProject.armVersion
    val scalaVersion = model.rootProject.buildScalaVersion

    val content =
      <span>
        <p><div class="explanation">
          <h3>SBT Example</h3>
          <ol>
            <li>Add core or file dependency to you project definition
              <div class="example_code">
                <pre class='brush: scala'>
                val core = "{groupId}" %% "core" % "{version}"
                val file = "{groupId}" %% "file" % "{version}"
                </pre>
              </div>
            </li>
            <li>update your SBT project to download the dependencies
              <pre>sbt update</pre>
            </li>
          </ol>

          There is an example SBT project prepackaged at <a href="https://github.com/downloads/jesseeichar/scala-io/example-sbt-project.zip">example-sbt-project.zip</a>.
          <pre><![CDATA[
    > curl -L https://github.com/downloads/jesseeichar/scala-io/example-sbt-project.zip > tmp.zip && unzip tmp.zip && rm tmp.zip
    > cd example-sbt-project
    > sbt update run]]>
          </pre>
        </div></p>
        <p><div class="explanation">
          <h3>Maven Example</h3>
          <ol>
            <li>Add core or file dependency to you project definition
              <div><pre>
{"""|<!-- Core -->
    |<dependency>
    | <groupId>%s</groupId>
    | <artifactId>core_2.8.1</artifactId>
    | <version>%s</version>
    |</dependency>""".stripMargin.format(groupId,version)}
{"""|<!-- File -->
    |<dependency>
    | <groupId>%s</groupId>
    | <artifactId>file_2.8.1</artifactId>
    | <version>%s</version>
    |</dependency>""".stripMargin.format(groupId,version)}
                </pre></div>
              Note that the scala version (_2.8.1) needs to be kept up to date manually unlike the sbt version
            </li>
            <li>Ensure that you have the scala tools repository added as one of your repositories in your pom.xml
              <pre>
{"""|<repositories>
    |  <repository>
    |    <id>scala-tools.org</id>
    |    <name>Scala-Tools Maven2 Repository</name>
    |    <url>http://scala-tools.org/repo-releases</url>
    |  </repository>
    |</repositories>""".stripMargin}
              </pre>
            </li>
          </ol>

          There is an example maven project prepackaged at <a href="https://github.com/downloads/jesseeichar/scala-io/example-maven-project.zip">example-maven-project.zip</a>.
          <pre><![CDATA[
    > curl -L https://github.com/downloads/jesseeichar/scala-io/example-maven-project.zip > tmp.zip && unzip tmp.zip && rm tmp.zip
    > cd example-maven-project
    > mvn compile exec:java -Dexec.mainClass="Main" ]]>
          </pre>
        </div></p>
        http://groups.google.com/group/scala-incubator
        <p><div class="explanation">
          <h3>Scala Bazaar installation</h3>
          Coming soon...
        </div></p>
        <p><div class="explanation">
          <h3>Manual Download Example</h3>
          Naturally a manual download option is also available.  The dependencies are fairly simple:
          <ul>
            <li>
              <a href={"http://scala-tools.org/repo-releases/com/github/jsuereth/scala-arm/scala-arm_"+scalaVersion+"/"+armVersion+"/scala-arm_"+scalaVersion+"-"+armVersion+".jar"}>Scala ARM</a>
            </li>
            <li>
              <a href={"http://scala-tools.org/repo-releases/"+groupId.replace(".","/")+"/core_"+scalaVersion+"/"+version+"/core_"+scalaVersion+"-"+version+".jar"}>Scala Core</a>
            </li>
            <li>
              <a href={"http://scala-tools.org/repo-releases/"+groupId.replace(".","/")+"/file_"+scalaVersion+"/"+version+"/core_"+scalaVersion+"-"+version+".jar"}>Scala File (Optional)</a>
            </li>
          </ul>
        </div></p>
      </span>

    Template(true, "Scala IO Getting Started Guide")(
        Template.cssAndJs("./"))(
        <h1>Scala IO Getting Started Guide</h1>)(
        content)(Template.rootNavbar(Link.GettingStarted,model.projectSites))
  }

}