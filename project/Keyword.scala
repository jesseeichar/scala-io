import sbt.IO
import java.nio.charset.Charset

case class Keyword(section:String,id:String,name:String,depth:Int,shortName:String,`type`:String,keywords:Set[String]) {
  override lazy val toString = """{"section":"%s","id":"%s","name":"%s","depth":%s,"shortName":"%s","type":"%s","keywords":"%s"}""".format(section,id,name,depth,shortName,`type`,keywords.mkString(" "))
}

object Keyword {
  val core = Keyword("core","index","Core",0,"Core","overview", Set("core"))
  val file = Keyword("file","index","File",0,"File","overview", Set("file"))
  
  def split(text:String):Set[String] = {
    val words = text.split("""\s+""").filter(_.trim.nonEmpty).toSet
    words.filter(Stopwords.EN contains _)
  }
  

}