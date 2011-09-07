import sbt.IO
import java.nio.charset.Charset

case class Keyword(section:String,id:String,name:String,depth:Int,shortName:String,`type`:String,keywords:Set[String]) {
  override lazy val toString = """{"section":"%s","id":"%s","name":"%s","depth":%s,"shortName":"%s","type":"%s","keywords":"%s"}""".format(section,id,name,depth,shortName,`type`,keywords.mkString(" "))
}

object Keyword {
  val gettingStarted = Keyword("getting-started","index","Getting Started Guide",0,"Getting Started","overview", Set("start introduction intro"))
  val roadmap = Keyword("roadmap","index","Scala IO Development Roadmap",0,"Roadmap","overview", Set("future map roadmap"))
  val overview = Keyword("overview","index","Scala IO Documentation ",0,"Overview","overview", Set("overview"))
  val performance = Keyword("performance","index","Performance Test Reports",0,"Performance","overview", Set("performance"))
  val core = Keyword("core","index","Scala IO Core API",0,"Core API","overview", Set("core"))
  val file = Keyword("file","index","Scala IO File API",0,"File API","overview", Set("file"))
  
  def split(text:String):Set[String] = {
    val words = text.split("""\s+""").filter(_.trim.nonEmpty).toSet
    words.filter(Stopwords.EN contains _)
  }
  

}