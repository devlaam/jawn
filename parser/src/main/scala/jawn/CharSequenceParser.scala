package org.typelevel.jawn

/**
 * Lazy character sequence parsing.
 *
 * This is similar to StringParser, but acts on character sequences.
 */
final private[jawn] class CharSequenceParser[J](cs: CharSequence) extends SyncParser[J] with CharBasedParser[J] {
  private[this] var _line = 0
  private[this] var offset = 0
  final protected[this] def column(i: Int): Int = i - offset
  final protected[this] def newline(i: Int): Unit = { _line += 1; offset = i + 1 }
  final protected[this] def line(): Int = _line
  final protected[this] def reset(i: Int): Int = i
  final protected[this] def checkpoint(state: Int, i: Int, context: FContext[J], stack: List[FContext[J]]): Unit = ()
  final protected[this] def at(i: Int): Char = cs.charAt(i)
  final protected[this] def at(i: Int, j: Int): CharSequence = cs.subSequence(i, j)
  final protected[this] def atEof(i: Int): Boolean = i == cs.length
  final protected[this] def close(): Unit = ()
}
