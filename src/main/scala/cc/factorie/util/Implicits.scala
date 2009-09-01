package cc.factorie.util

import scala.util.Random
import scala.util.Sorting
import scala.reflect.Manifest
import java.io.File
import cc.factorie.util._

/**Implicit conversions, some but not all from scalanlp, Copyright 2009 David Hall, Daniel Ramage  */

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


object Implicits {
	implicit def stringExtras(s: String) = new {
		/**Implements Levenshtein Distance, with specific operation costs to go from this to s2.  Original version was from scalanlp. */
		def editDistance(s2: String, substCost: Int, deleteCost: Int, insertCost: Int): Int = {
			if (s.length == 0) s2.length
			else if (s2.length == 0) s.length
			else {
				val d = new Array[Array[Int]](s.length + 1, s2.length + 1)
				for (i <- 0 to s.length)
					d(i)(0) = i * deleteCost
				for (i <- 0 to s2.length)
					d(0)(i) = i * insertCost
				for (i <- 1 to s.length; j <- 1 to s2.length) {
					val cost = if (s(i - 1) == s2(j - 1)) 0 else substCost
					d(i)(j) = Math.min(d(i - 1)(j) + deleteCost, Math.min(d(i)(j - 1) + insertCost, d(i - 1)(j - 1) + cost))
				}
				d(s.length)(s2.length)
			}
		}

		def editDistance(s2: String): Int = editDistance(s2, 1, 1, 1)
	}

 
 
	implicit def iterableExtras[T](s: Iterable[T]) = new {
		def sum(extractor: T => Double): Double = s.foldLeft(0.0)((sum, x: T) => sum + extractor(x))
		// TODO I would love to change "sumInts" to simply "sum" but the type inferencer seems to have trouble with seq.sum(_ score)
		def sumInts(extractor: T => Int): Int = s.foldLeft(0)((sum, x: T) => sum + extractor(x))

		def product(extractor: T => Double): Double = s.foldLeft(1.0)((prod, x) => prod * extractor(x))

		def productInts(extractor: T => Int): Int = s.foldLeft(1)((prod, x) => prod * extractor(x))

		def max(extractor: T => Double): T = {
			val xs = s.elements
			if (!xs.hasNext) throw new IllegalArgumentException("<empty>.max((x:T)=>Double)")
			var maxElement = xs.next
			var maxValue = extractor(maxElement)
			while (xs.hasNext) {
				val x = xs.next
				val v = extractor(x)
				if (v > maxValue) {
					maxElement = x
					maxValue = v
				}
			}
			maxElement
		}

		def maxInt(extractor: T => Int): T = {
			val xs = s.elements
			if (!xs.hasNext) throw new IllegalArgumentException("<empty>.maxInt((x:T)=>Int)")
			var maxElement = xs.next
			var maxValue = extractor(maxElement)
			while (xs.hasNext) {
				val x = xs.next
				val v = extractor(x)
				if (v > maxValue) {
					maxElement = x
					maxValue = v
				}
			}
			maxElement
		}

		def min(extractor: T => Double): T = {
			val xs = s.elements
			if (!xs.hasNext) throw new IllegalArgumentException("<empty>.max((x:T)=>Double)")
			var minElement = xs.next
			var minValue = extractor(minElement)
			while (xs.hasNext) {
				val x = xs.next
				val v = extractor(x)
				if (v < minValue) {
					minElement = x
					minValue = v
				}
			}
			minElement
		}

		def minInt(extractor: T => Int): T = {
			val xs = s.elements
			if (!xs.hasNext) throw new IllegalArgumentException("<empty>.minInt((x:T)=>Int)")
			var minElement = xs.next
			var minValue = extractor(minElement)
			while (xs.hasNext) {
				val x = xs.next
				val v = extractor(x)
				if (v < minValue) {
					minElement = x
					minValue = v
				}
			}
			minElement
		}

		def sumAndMin(extractor: T => Double): (Double, T) = {
			var sum = 0.0
			val xs = s.elements
			if (!xs.hasNext) throw new IllegalArgumentException("<empty>.max((x:T)=>Double)")
			var minElement = xs.next
			var minValue = extractor(minElement)
			sum += minValue
			while (xs.hasNext) {
				val x = xs.next
				val v = extractor(x)
				sum += v
				if (v < minValue) {
					minElement = x
					minValue = v
				}
			}
			(sum, minElement)
		}

		/**Returns both the maximum element and the second-to-max element */
		// TODO reimplement this to make it more efficient; no need to sort the whole sequence
		def max2(extractor: T => Double): (T, T) = {
			val s1 = s.toSeq
			assert(s1.length > 1)
			val s2: Seq[T] = Sorting.stableSort(s1, (x1: T, x2: T) => extractor(x1) > extractor(x2))
			(s2(0), s2(1))
		}
		//def filterByClass[X](implicit m:Manifest[X]) = s.filter(x:T => m.erasure.isAssignableFrom(x.getClass)).asInstanceOf[Seq[X]]

		/**Sorts with maximum first.*/
		def sortReverse(extractor: T => Double): Seq[T] =
			Sorting.stableSort(s.toSeq, (x1: T, x2: T) => extractor(x1) > extractor(x2))

		def sample(implicit random: Random): T = {
			val s2 = s.toSeq
			s2(random.nextInt(s2.size))
		}
  
		def sampleFiltered(filterTest: T => Boolean)(implicit random: Random): T = {
		  val s2 = s.toSeq.filter(filterTest)
		  s2(random.nextInt(s2.size));
		}

		def sample(extractor: T => Double)(implicit random: Random): T = {
			var sum = s.foldLeft(0.0)((total, x) => total + extractor(x))
			val r = random.nextDouble * sum
			sum = 0
			for (choice <- s) {
				val e = extractor(choice)
				if (e < 0.0) throw new Error("BonusIterable sample extractor value " + e + " less than zero.  Sum=" + sum)
				sum += e
				if (sum >= r)
					return choice;
			}
			throw new Error("BonusIterable sample error: r=" + r + " sum=" + sum)
		}

		def shuffle(implicit random: Random) = {
			val s2 = s.map(x => (x, random.nextInt)).toSeq
			Sorting.stableSort(s2, (t1: (T, int), t2: (T, int)) => t1._2 > t2._2).map(t => t._1)
		}

		def split(ratio: Double): (Seq[T], Seq[T]) = {
			val s2 = s.toSeq
			if (ratio <= 0 || ratio > 1.0) throw new Error
			val index = (ratio * s2.size).toInt
			if (index >= s2.size)
				(s2, Seq.empty)
			else
				(s2.slice(0, index), s2.drop(index))
		}

		def filterByType[S](implicit m: Manifest[S]): Iterable[S] =
			for (x <- s; if (m.erasure.isAssignableFrom(x.asInstanceOf[AnyRef].getClass))) yield x.asInstanceOf[S]

		def filterByClass[C](c: Class[C]): Iterable[C] =
			for (x <- s; if (c.isAssignableFrom(x.asInstanceOf[AnyRef].getClass))) yield x.asInstanceOf[C]
	}

	/*
	implicit def listExtras[A](list:List[A]) = new {
		def reverseForeach(f: A => Unit): Unit = {
			def loop(l: List[A]): Unit = l match {
				case Nil =>
				case head :: tail => { loop(tail); f(head) }
			}
			loop(list)
		}
	}
*/

	implicit def fileExtras(file: File) = new {
		import java.io.{ByteArrayOutputStream, FileInputStream}
		def contentsAsString: String = {
			val bos = new ByteArrayOutputStream
			val ba = new Array[Byte](2048)
			val is = new FileInputStream(file)
			def read {
				is.read(ba) match {
					case n if n < 0 =>
					case 0 => read
					case n => bos.write(ba, 0, n); read
				}
			}
			read
			bos.toString("UTF-8")
		}
	}

	/**Provides extensions to Any. */
	implicit def anyExtras[T <: AnyRef](t: T) = new {
		/**if t is non-null, return it, otherwise, evaluate and return u. */
		def ?:[U >: T](u: => U) = if (t eq null) u else t;

		/**Intern for arbitrary types */
		def intern: T = {
			val in: Interner[T] = Interner.forClass(t.getClass.asInstanceOf[Class[T]])
			in(t);
		}

		/**if t is non-null return Some(t), otherwise None */
		def toOption = if (t eq null) None else Some(t);
	}

	implicit def doubleExtras(d: Double) = new {
		def =~=(e: Double) = d == e || Math.abs(d - e) / d < 1E-4;
	}

	implicit def seqExtras[T](s: Seq[T]) = new {
		// useful subset selection
		def apply(x: Seq[Int]) = x.projection.map(s);
	}

	implicit def randomExtras(r: Random) = new {
		def flip(p: Double) = r.nextDouble < p
	}

}

/*
  class BonusSource(s:Source) {
    def asString = s.getLines.foldLeft(new StringBuffer)(_ append _).toString
  }
  implicit def bonusSource(s:Source) = new BonusSource(s)
*/

