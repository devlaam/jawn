package org.typelevel.jawn

import scala.annotation.switch

/**
 * Trait used when the data to be parsed is in UTF-16.
 *
 * This parser provides parseString(). Like ByteBasedParser it has
 * fast/slow paths for string parsing depending on whether any escapes
 * are present.
 *
 * It is simpler than ByteBasedParser.
 */
trait CharBasedParser[J] extends Parser[J] {

  final protected[this] val builder = new StringBuilder()

  /**
   * This method expects the data to be in UTF-16 and accesses it as chars.
   * It performs the correct checks to make sure that we don't
   * interpret a multi-char code point incorrectly.
   * Switches to (more expensive) escaped string parsing when required.
   */
  protected[this] final def parseString(i: Int, continue: (Char,=>Char)=>Boolean, kill: (Char)=>Boolean): Int = {
    var j = i
    var esc = false
    var c = at(j)
    val sb = builder
    sb.setLength(0)

    while ( continue(c, at(j+1)) ) {
      if (kill(c.toChar)) {
        die(j, s"control char (${c.toInt}) in string", 1)
      } else if (c == '\\') {
        if (!esc) { sb.append(at(i, j)); esc = true }
        (at(j+1): @switch) match {
          case 'b'  => { sb.append('\b'); j += 2 }
          case 'f'  => { sb.append('\f'); j += 2 }
          case 'n'  => { sb.append('\n'); j += 2 }
          case 'r'  => { sb.append('\r'); j += 2 }
          case 't'  => { sb.append('\t'); j += 2 }
          case '"'  => { sb.append('"');  j += 2 }
          case '/'  => { sb.append('/');  j += 2 }
          case '\\' => { sb.append('\\'); j += 2 }
          // if there's a problem then descape will explode
          case 'u'  => { sb.append(descape(j + 2, at(j + 2, j + 6))); j += 6 }
          case  e   => die(j, s"illegal escape sequence (\\$e)", 1)
        }
      } else {
        if (esc) sb.append(c)
        j += 1
      }
      c = at(j)
    }
    j
  }
}
