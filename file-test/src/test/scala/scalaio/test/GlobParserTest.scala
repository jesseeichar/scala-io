package scalaio.test

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import org.junit.Assert._
import org.junit.Test
import scalax.file.ramfs.RamFileSystem
import scalax.file.FileSystem
import scalax.file.GlobParser
import java.util.regex.Pattern

class GlobParserTest {
  def compareGlob(glob:String, regex:String)(implicit fs:FileSystem) {
    val parser = new GlobParser(fs)
    val result = parser(glob)
    assertEquals("expected '"+glob+"' to be parsed into '"+regex+"' but got '"+result+"'", regex, result)
  }
  @Test
  def testGlobToRegEx :Unit = {
    val sep = "/"
    implicit val fs = new RamFileSystem(separator=sep)

    val noSep = "[^"+sep+"]"
    compareGlob("**",".*")
    compareGlob("*","[^"+sep+"]*")
    compareGlob(""+sep+"*",sep+noSep+"*")
    compareGlob(""+sep+"**",""+sep+".*")
    compareGlob("\\*","\\*")
    compareGlob("/?","/"+noSep)
    compareGlob("h\\?","\\Qh\\E\\?")
    compareGlob("[a-z.]","[a-z.]")
    compareGlob("[!a-z.]","[^a-z.]")
    compareGlob("[!-]","[^-]")
    compareGlob("[-ab\\\\\\]]","[-ab\\\\\\]]")
    compareGlob("{h,he,l, escape?\\}}",List("h","he","l","escape?}") map (Pattern.quote) mkString ("(","|",")"))
    compareGlob("hi"+sep+"o**k","\\Qhi\\E\\Q"+sep+"\\E\\Qo\\E"+noSep+"*"+noSep+"*\\Qk\\E")
    compareGlob("hi"+sep+"o\\?","\\Qhi\\E\\Q"+sep+"\\E\\Qo\\E\\?")
    compareGlob("hi"+sep+"o\\"+sep+"hi"+sep,"\\Qhi\\E\\Q"+sep+"\\E\\Qo\\E\\"+sep+"\\Qhi\\E\\Q"+sep+"\\E")
    compareGlob("th.s"+sep+"\\\\ws"+sep,"\\Qth.s\\E\\Q"+sep+"\\E\\\\\\Qws\\E\\Q"+sep+"\\E")
    compareGlob("hi"+sep+"**"+sep+"[a-b]*.{scala,java}","\\Qhi\\E\\Q"+sep+"\\E.*\\Q"+sep+"\\E[a-b]"+noSep+"*\\Q.\\E(\\Qscala\\E|\\Qjava\\E)")

  }
}
