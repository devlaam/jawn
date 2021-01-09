package jawn

import scala.collection.mutable

trait CocoFacade[J] extends Facade[J]
{ def jarray(vs: List[J]): J
  def jobject(vs: List[(String, J)]): J

  def singleContext() = new FContext[J]
  { var value: J = _
    //def add(s: CharSequence) { value = jstring(s) }
    def add(s: CharSequence)= {}
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false }

  def arrayContext() = new FContext[J]
  { val vs = mutable.ListBuffer.empty[J]
    //def add(s: CharSequence) { vs += jstring(s) }
    def add(s: CharSequence)= {}
    def add(v: J) { vs += v }
    def finish: J = jarray(vs.toList)
    def isObj: Boolean = false }

  def objectContext() = new FContext[J]
  { var keyStr: String = null
    var comStr: String = null
    var vs = mutable.ListBuffer.empty[(String,J)]
    override def key(s: CharSequence)     { keyStr = s.toString }
    override def comment(s: CharSequence) { comStr = s.toString; println("Found comment = "+comStr) }
    //override def comment(s: CharSequence) { comStr = s.toString }
    //def add(s: CharSequence): Unit = if (key == null) { key = s.toString } else { vs += key -> jstring(s); key = null }
    def add(s: CharSequence)= {}
    def add(v: J)                { if (keyStr != null) vs += keyStr -> v; keyStr = null }
    def finish = jobject(vs.toList)
    def isObj = true } }


object CocoAst extends CocoFacade[Unit]
{ def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = jnum(s.toString)
  def jstring(s: CharSequence)                            = jstring(s.toString)

  def jnull()                              = {}
  def jfalse()                             = {}
  def jtrue()                              = {}
  def jnum(s: String)                      = { println("Found number = "+s) }
  def jstring(s: String)                   = { println("Found string = "+s) }
  def jarray(vs: List[Unit])               = {}
  def jobject(vs: List[(String, Unit)])    = {} }


object ManualTest extends App
{
   val source1 = ( """
    { // Multiline op een line 0
      "number" : 42,
      /* Multiline op een line 1 */
      "string" : "FooBar",
      /* Multiline op twee
        regels line 2 */
      "empobj" : { },
      "emparr" : [ ],
      /* Commentaar over
         meerdere
         regels */
      "obj1"   : { "een": 1 },
      "arr1"   : [ "1" ],
      // Commentaar op een regel
      "obj3"   : { "een" : 1.0 , "twee" : 2 , "drie" : 3.40 },
      "arr3"   : [ "1" , "2" , "3" ],
      "mix1"   :
      { "een": 1,
        "twee": { "name" : "Jan", /* Tussen de regels */"age" : 23, "id" : true },
        "drie": { "drie" : "3" } },
      "mix2"   :
      { "een": 1,
        "twee":
        { "name" : "Jan",
          "ages" : [23,24,25,25,null],
          "id" : true },
        "drie": { "drie" : "3" } },
      /* Multiline op een line 3 */
      "numbs"  :
      [ { "een": "1" } ,
        { "twee": "2" } ,
        { "drie":"3" } ],
      "membs"  :
      [ { "name": "Jan",  "age": 23, "id": true },
        { "name": "Piet", "age": 43, "id": true },
        { "name": "Klaas", "age": 19, "id": false } ],
      "number" : 43 }
    """)


   val source2 = ( """
    { /* This is form the image library:
         "examples dot com". */
        "Image": {
          "Width":  800,
          "Height": 600,
          "Title":  "View from 15th Floor",
          // Thumbnails should not exceed 320x240 pixels.
          "Thumbnail": {
            "Url":    "http://www.example.com/image/481989943",
            "Height": 125,
            "Width":  100
          },
        "Animated" : false,
        "IDs": [116, 943, 234, 38793]
      }
    }
   """)

  Parser.parseFromString(source1)(CocoAst)
  Parser.parseFromString(source2)(CocoAst)

}
