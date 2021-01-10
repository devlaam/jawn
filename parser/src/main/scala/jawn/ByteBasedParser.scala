package org.typelevel.jawn

import scala.annotation.switch

/**
 * Trait used when the data to be parsed is in UTF-8.
 *
 * This parser has to translate input bytes to Chars and Strings. It
 * provides a byte() method to access individual bytes, and also
 * parser strings from bytes.
 *
 * Its parseString() implementation has two cases. In the first case
 * (the hot path) the string has no escape sequences and we can just
 * UTF-8 decode the entire set of bytes. In the second case, it goes
 * to some trouble to be sure to de-escape correctly given that the
 * input data is UTF-8.
 */
trait ByteBasedParser[J] extends Parser[J] {
  protected[this] def byte(i: Int): Byte

  final protected[this] val builder = new StringBuilder()

  /**
   * This method expects the data to be in UTF-8 and accesses it as bytes.
   * Thus we can just ignore any bytes with the highest bit set.
   * It performs the correct checks to make sure that we don't
   * interpret a multi-char code point incorrectly.
   * Switches to (more expensive) escaped string parsing when required.
   */
  final protected[this] def parseString(i: Int, continue: (Char,=>Char)=>Boolean, kill: (Char)=>Boolean): Int = {
    var j = i
    var esc = false
    val sb = builder
    sb.setLength(0)
    var c: Int = byte(j) & 0xff

    while ( continue(c.toChar, (byte(j+1) & 0xff).toChar) ) {
      if (kill(c.toChar)) {
        die(j, s"control char ($c) not allowed here", 1)
      } else if (c == 92) {
        if (!esc) { sb.append(at(i, j)); esc = true }
        ((byte(j+1) & 0xff): @switch) match {
          case  98 => { sb.append('\b'); j += 2 }
          case 102 => { sb.append('\f'); j += 2 }
          case 110 => { sb.append('\n'); j += 2 }
          case 114 => { sb.append('\r'); j += 2 }
          case 116 => { sb.append('\t'); j += 2 }
          case  34 => { sb.append('"');  j += 2 }
          case  47 => { sb.append('/');  j += 2 }
          case  92 => { sb.append('\\'); j += 2 }
          // if there's a problem then descape will explode
          case 117 => { sb.append(descape(j + 2, at(j + 2, j + 6))); j += 6 }
          case   e => die(j, s"illegal escape sequence (\\${e.toChar})", 1)
        }
      } else if (c < 128) {
        // 1-byte UTF-8 sequence
        if (esc) sb.append(c.toChar)
        j += 1
      } else if ((c & 224) == 192) {
        // 2-byte UTF-8 sequence
        if (esc) sb.append(at(j, j + 2))
        j += 2
      } else if ((c & 240) == 224) {
        // 3-byte UTF-8 sequence
        if (esc) sb.append(at(j, j + 3))
        j += 3
      } else if ((c & 248) == 240) {
        // 4-byte UTF-8 sequence
        if (esc) sb.append(at(j, j + 4))
        j += 4
      } else {
        die(j, "invalid UTF-8 encoding")
      }
      c = byte(j) & 0xff
    }
    j
  }
}
