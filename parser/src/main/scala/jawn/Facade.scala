package org.typelevel.jawn

import scala.collection.mutable

/**
 * [[Facade]] is a type class that describes how Jawn should construct
 * JSON AST elements of type `J`.
 *
 * `Facade[J]` also uses `FContext[J]` instances, so implementors will
 * usually want to define both.
 */
trait Facade[J] {
  def singleContext(index: Int): FContext[J]
  def arrayContext(index: Int): FContext[J]
  def objectContext(index: Int): FContext[J]

  def jnull(index: Int): J
  def jfalse(index: Int): J
  def jtrue(index: Int): J
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J
  def jstring(s: CharSequence, index: Int): J
  def jstring(s: CharSequence, start: Int, limit: Int): J = jstring(s, start)
}

<<<<<<< HEAD
/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
trait FContext[J] extends RawFContext[J]{
  /* In order not to break existing facades we call @deprecated add.
   * Override these to enable the new logic with keys and comments. */
  def key(s: CharSequence): Unit = add(s)
  def comment(s: CharSequence): Unit = add(s)

  @deprecated("this method is scheduled for removal","0.12.2-f1")
  def add(s: CharSequence): Unit

  def add(v: J): Unit
  def finish(): J

  def key(s: CharSequence, index: Int): Unit = key(s)
  def comment(s: CharSequence, index: Int): Unit = comment(s)
  def add(v: J, index: Int) = add(v)
  def finish(index: Int) = finish()
=======
object Facade {

  /**
   * A convenience trait for [[Facade]] implementers who don't need character offsets.
   */
  trait NoIndexFacade[J] extends Facade[J] {
    def singleContext(): FContext[J]
    def arrayContext(): FContext[J]
    def objectContext(): FContext[J]

    def jnull: J
    def jfalse: J
    def jtrue: J
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int): J
    def jstring(s: CharSequence): J
>>>>>>> master

    final def singleContext(index: Int): FContext[J] = singleContext()
    final def arrayContext(index: Int): FContext[J] = arrayContext()
    final def objectContext(index: Int): FContext[J] = objectContext()

<<<<<<< HEAD
/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
trait RawFContext[J] {
  def key(s: CharSequence, index: Int): Unit
  def comment(s: CharSequence, index: Int): Unit

  def add(v: J, index: Int): Unit
  def finish(index: Int): J
  def isObj: Boolean
=======
    final def jnull(index: Int): J = jnull
    final def jfalse(index: Int): J = jfalse
    final def jtrue(index: Int): J = jtrue
    final def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J =
      jnum(s, decIndex, expIndex)
    final def jstring(s: CharSequence, index: Int): J = jstring(s)
    final override def jstring(s: CharSequence, start: Int, limit: Int): J = jstring(s)
  }

  /**
   * A convenience trait for [[Facade]] implementers that doesn't require [[FContext]] implementations.
   */
  trait SimpleFacade[J] extends NoIndexFacade[J] {
    def jarray(vs: List[J]): J
    def jobject(vs: Map[String, J]): J

    final def singleContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] var value: J = _
        def add(s: CharSequence): Unit = value = jstring(s)
        def add(v: J): Unit = value = v
        def finish(): J = value
        def isObj: Boolean = false
      }

    final def arrayContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] val vs = mutable.ListBuffer.empty[J]
        def add(s: CharSequence): Unit = vs += jstring(s)
        def add(v: J): Unit = vs += v
        def finish(): J = jarray(vs.toList)
        def isObj: Boolean = false
      }

    final def objectContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] var key: String = null
        private[this] var vs = Map.empty[String, J]
        def add(s: CharSequence): Unit =
          if (key == null)
            key = s.toString
          else {
            vs = vs.updated(key, jstring(s)); key = null
          }
        def add(v: J): Unit = { vs = vs.updated(key, v); key = null }
        def finish(): J = jobject(vs)
        def isObj: Boolean = true
      }
  }

  /**
   * A convenience trait for [[Facade]] implementers that doesn't require [[FContext]] implementations
   * and uses mutable collections.
   */
  trait MutableFacade[J] extends NoIndexFacade[J] {
    def jarray(vs: mutable.ArrayBuffer[J]): J
    def jobject(vs: mutable.Map[String, J]): J

    final def singleContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] var value: J = _
        def add(s: CharSequence): Unit = value = jstring(s)
        def add(v: J): Unit = value = v
        def finish(): J = value
        def isObj: Boolean = false
      }

    final def arrayContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] val vs = mutable.ArrayBuffer.empty[J]
        def add(s: CharSequence): Unit = vs += jstring(s)
        def add(v: J): Unit = vs += v
        def finish(): J = jarray(vs)
        def isObj: Boolean = false
      }

    final def objectContext(): FContext[J] =
      new FContext.NoIndexFContext[J] {
        private[this] var key: String = null
        private[this] val vs = mutable.Map.empty[String, J]
        def add(s: CharSequence): Unit =
          if (key == null)
            key = s.toString
          else {
            vs(key) = jstring(s); key = null
          }
        def add(v: J): Unit = { vs(key) = v; key = null }
        def finish(): J = jobject(vs)
        def isObj: Boolean = true
      }
  }

  /**
   * [[NullFacade]] discards all JSON AST information.
   *
   * This is the simplest possible facade. It could be useful for
   * checking JSON for correctness (via parsing) without worrying about
   * saving the data.
   *
   * It will always return `()` on any successful parse, no matter the
   * content.
   */
  object NullFacade extends NoIndexFacade[Unit] {
    private[this] val nullContext: FContext[Unit] = new FContext.NoIndexFContext[Unit] {
      def add(s: CharSequence): Unit = ()
      def add(v: Unit): Unit = ()
      def finish(): Unit = ()
      def isObj: Boolean = false
    }

    private[this] val nullObjectContext: FContext[Unit] = new FContext.NoIndexFContext[Unit] {
      def add(s: CharSequence): Unit = ()
      def add(v: Unit): Unit = ()
      def finish(): Unit = ()
      def isObj: Boolean = true
    }

    def singleContext(): FContext[Unit] = nullContext
    def arrayContext(): FContext[Unit] = nullContext
    def objectContext(): FContext[Unit] = nullObjectContext

    def jnull: Unit = ()
    def jfalse: Unit = ()
    def jtrue: Unit = ()
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Unit = ()
    def jstring(s: CharSequence): Unit = ()
  }
>>>>>>> master
}
