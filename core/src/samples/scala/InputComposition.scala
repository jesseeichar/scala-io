/**
 * These examples show various strategies for composing files together
 */
object InputComposition {

  /**
   * Zip the characters of two files together and find all that don't match
   */
  def zipAndCompare {
    import scalax.io.Resource
    
    val googleCom = Resource.fromURL("http://google.com").chars
    val googleCH = Resource.fromURL("http://google.ch").chars
    
    googleCom.zip(googleCH).filter{case (com,ch) => com != ch}
  }
  
  /**
   * Compare two inputs by comparing the first byte of each 100 byte block  
   */
  def blockCompare {
    import scalax.io.Resource

    val googleCom = Resource.fromURL("http://google.com").bytes
    val googleCH = Resource.fromURL("http://google.ch").bytes
    
    val blocks = googleCom.sliding(100,100).zip(googleCH.sliding(100,100))
    
    blocks.forall{case (com,ch) => com.head == ch.head}
  }
  
}