package jawn

import scala.annotation.{switch, tailrec}

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

  protected[this] final val charBuilder = new CharBuilder()

  /**
   * This method expects the data to be in UTF-16 and accesses it as chars.
   * It performs the correct checks to make sure that we don't
   * interpret a multi-char code point incorrectly.
   * Switches to (more expensive) escaped string parsing when required.
   */
  protected[this] final def parseString(i: Int, ctxt: RawFContext[J], continue: (Char,=>Char)=>Boolean, kill: (Char)=>Boolean): Int = {
    var j = i
    var esc = false
    val sb = charBuilder.reset
    var c = at(j)

    while ( continue(c, at(j+1)) ) {
      if (kill(c.toChar)) {
        die(j, s"control char (${c.toInt}) not allowed here")
      } else if (c == '\\') {
        if (!esc) { sb.extend(at(i, j)); esc = true }
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
          case 'u'  => { sb.append(descape(at(j + 2, j + 6))); j += 6 }
          case  e   => die(j, s"illegal escape sequence (\\$e)")
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
