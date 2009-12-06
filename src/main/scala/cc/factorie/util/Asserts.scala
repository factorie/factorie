/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util

;

/**
 * Collection of asserts that give more useful error messages.
 *
 * @author dlwh
 */
trait Asserts {
  /**== */
  def assertEq[A, B](a: A, b: B) = assert(a == b, a + "is not == " + b);

  /**!= */
  def assertNe[A <% Ordered[B], B](a: A, b: B) = assert(a != b, a + "is not != " + b);

  /**&lt; */
  def assertLt[A <% Ordered[B], B](a: A, b: B) = assert(a < b, a + "is not < " + b);

  /**&lt;= */
  def assertLe[A <% Ordered[B], B](a: A, b: B) = assert(a <= b, a + "is not <= " + b);

  /**&gt; */
  def assertGt[A <% Ordered[B], B](a: A, b: B) = assert(a > b, a + "is not > " + b);

  /**&gt;= */
  def assertGe[A <% Ordered[B], B](a: A, b: B) = assert(a >= b, a + "is not >= " + b);
}


/**
 * Collection of asserts that give more useful error messages.
 *
 * @author dlwh
 */
object Asserts extends Asserts;
