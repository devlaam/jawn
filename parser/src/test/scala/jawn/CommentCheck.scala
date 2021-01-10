package org.typelevel.jawn
package parser

import org.scalacheck.Properties
import org.typelevel.claimant.Claim

class CommentCheck extends Properties("CommentCheck") {

  trait TestFacade[J] extends Facade.NoIndexFacade[J] {
    def jarray(vs: List[J]): J
    def jobject(vs: List[(String, J)]): J

    def jerror(s: String): Unit
    def jcomment(key: String, text: String): Unit

    protected class SingleContext extends FContext.NoIndexFContext[J] {
      private var value: J = _
      def add(s: CharSequence): Unit = jerror("CharSequence on the loose in SingleContext")
      def add(v: J): Unit =  { value = v }
      def finish(): J = value
      def isObj: Boolean = false
      }

    protected class ArrayContext extends FContext.NoIndexFContext[J] {
      private var vs: List[J] = Nil
      def add(s: CharSequence) =  jerror("CharSequence on the loose in ArrayContext")
      def add(v: J): Unit = { vs ::= v }
      def finish(): J = jarray(vs.reverse)
      def isObj: Boolean = false
      }

    protected class ObjectContext extends FContext.NoIndexFContext[J] {
      private var keyStr: Option[String] = None
      private var comStr: Option[String] = None
      private var vs: List[(String,J)] = Nil
      override def key(s: CharSequence): Unit = { keyStr = Some(s.toString); comStr.foreach(jcomment(s.toString,_)); comStr = None }
      override def comment(s: CharSequence): Unit = { comStr = Some(s.toString) }
      def add(s: CharSequence): Unit = jerror("CharSequence on the loose in ObjectContext")
      def add(v: J): Unit =  { keyStr.foreach(k => vs ::= ( (k,v) ) );  keyStr = None }
      def finish(): J = jobject(vs.reverse)
      def isObj = true
      }

    def singleContext() = new SingleContext
    def arrayContext = new ArrayContext
    def objectContext() = new ObjectContext
    }

  class TestPrinter extends TestFacade[Unit] {
    private var collect: List[String] = Nil

    def digest = collect.reverse.flatMap(_.split("\n")).map(_.trim)

    def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Unit = jnum(s.toString)
    def jstring(s: CharSequence): Unit                            = jstring(s.toString)

    def jnull: Unit                                = {}
    def jfalse: Unit                               = {}
    def jtrue: Unit                                = {}
    def jnum(s: String): Unit                      = {}
    def jstring(s: String): Unit                   = {}
    def jarray(vs: List[Unit]): Unit               = {}
    def jobject(vs: List[(String, Unit)]): Unit    = {}

    def jerror(s: String): Unit                    = { collect ::= s"Found error: '$s'" }
    def jcomment(key: String, text: String): Unit  = { collect ::= s"Found comment: '$text' on key: '$key'" }
    }


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
      "number" : 43 } """)

  val source2 = ( """
    { /* This is from the image library:
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
    }  """)

  val result1 = List(
    "Found comment: ' Multiline op een line 0' on key: 'number'",
    "Found comment: ' Multiline op een line 1 ' on key: 'string'",
    "Found comment: ' Multiline op twee",
    "regels line 2 ' on key: 'empobj'",
    "Found comment: ' Commentaar over",
    "meerdere",
    "regels ' on key: 'obj1'",
    "Found comment: ' Commentaar op een regel' on key: 'obj3'",
    "Found comment: ' Tussen de regels ' on key: 'age'",
    "Found comment: ' Multiline op een line 3 ' on key: 'numbs'")

  val result2 = List(
    "Found comment: ' This is from the image library:",
    "\"examples dot com\". ' on key: 'Image'",
    "Found comment: ' Thumbnails should not exceed 320x240 pixels.' on key: 'Thumbnail'")

  property("test on json 1") = {
    val testPrinter = new TestPrinter
    Parser.parseFromString(source1)(testPrinter)
    Claim(testPrinter.digest == result1) }

  property("test on json 2") = {
    val testPrinter = new TestPrinter
    Parser.parseFromString(source2)(testPrinter)
    Claim(testPrinter.digest == result2) }


}
