package org.typelevel.jawn

/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
trait FContext[J] {
  
  /* In order not to break existing facades we call add.
   * Override these to enable the new logic with keys and comments. 
   * In that case you can leave the implemention of add empty. */
  def key(s: CharSequence, index: Int): Unit = add(s, index)
  def comment(s: CharSequence, index: Int): Unit  = add(s, index)
  /* This method is only called via the methods above in this branch. */
  def add(s: CharSequence, index: Int): Unit
  /* This method is not called in this branch. */
  def add(s: CharSequence, start: Int, limit: Int): Unit = add(s, start)
  def add(v: J, index: Int): Unit
  def finish(index: Int): J
  def isObj: Boolean

 }

object FContext {

  /**
   * A convenience trait for implementers who don't need character offsets.
   */
  trait NoIndexFContext[J] extends FContext[J] {
    /* In order not to break existing facades we call add.
     * Override these to enable the new logic with keys and comments. */
    def key(s: CharSequence): Unit = add(s)
    def comment(s: CharSequence): Unit = add(s)
    /* This method is only called via the methods above in this branch. */
    def add(s: CharSequence): Unit
    def add(v: J): Unit
    def finish(): J

    final def add(s: CharSequence, index: Int): Unit = add(s)
    final def add(v: J, index: Int): Unit = add(v)
    final def finish(index: Int): J = finish()
    
    final override def key(s: CharSequence, index: Int): Unit = key(s)
    final override def comment(s: CharSequence, index: Int): Unit = comment(s)
  }
}
