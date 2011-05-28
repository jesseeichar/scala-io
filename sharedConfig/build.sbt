organization := "com.github.scala-incubator.io"

version := "0.2.0-SNAPSHOT"

maxErrors := 20

scalacOptions += "-deprecation"

offline := false

scalaVersion := "2.9.0-1"

publishTo <<= (version) { version => 
  if(version.toString endsWith "-SNAPSHOT")
    Some("nexus.scala-tools.org" at "http://nexus.scala-tools.org/content/repositories/snapshots/")
  else
    Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

pomExtra :=
<licenses>
  <license>
    <name>Scala License</name>
    <url>http://www.scala-lang.org/node/146</url>
    <distribution>repo</distribution>
  </license>
</licenses>
