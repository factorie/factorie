package cc.factorie
{

/*
Directly adapted from:
Michael Wichura,The Percentage Points of the Normal Distribution,Applied Statistics, Volume 37, Number 3, pages 477-484, 1988.
Algorithm AS 241,
*/
object Maths
   {
	val a = List(
		3.3871328727963666080,
		133.14166789178437745,
		1971.5909503065514427,
		13731.693765509461125,
		45921.953931549871457,
		67265.770927008700853,
		33430.575583588128105,
		2509.0809287301226727
		).toArray;
	val b = List(
		1.0,
		42.313330701600911252,
		687.18700749205790830,
		5394.1960214247511077,
		21213.794301586595867,
		39307.895800092710610,
		28729.085735721942674,
		5226.4952788528545610
		).toArray;
	val c = List(
		1.42343711074968357734,
		4.63033784615654529590,
		5.76949722146069140550,
		3.64784832476320460504,
		1.27045825245236838258,
		0.241780725177450611770,
		0.0227238449892691845833,
		0.00077454501427834140764
		).toArray;

	val const1 = 0.180625;
	val const2 = 1.6;

	val d = List(
		1.0E+00,
		2.05319162663775882187E+00,
		1.67638483018380384940E+00,
		6.89767334985100004550E-01,
		1.48103976427480074590E-01,
		1.51986665636164571966E-02,
		5.47593808499534494600E-04,
		1.05075007164441684324E-09
		).toArray;
	val e = List(
		6.65790464350110377720E+00,
		5.46378491116411436990E+00,
		1.78482653991729133580E+00,
		2.96560571828504891230E-01,
		2.65321895265761230930E-02,
		1.24266094738807843860E-03,
		2.71155556874348757815E-05,
		2.01033439929228813265E-07
		).toArray;
	val f = List(
		1.0E+00,
		5.99832206555887937690E-01,
		1.36929880922735805310E-01,
		1.48753612908506148525E-02,
		7.86869131145613259100E-04,
		1.84631831751005468180E-05,
		1.42151175831644588870E-07,
		2.04426310338993978564E-15
		).toArray;
	val split1 = 0.425;
	val split2 = 5.0;


	def probit(p: Double): Double =
		{
			if (p <= 0)
				return java.lang.Double.NEGATIVE_INFINITY;
			if (p >= 1)
				return java.lang.Double.POSITIVE_INFINITY;
			val q = p - 0.5;
			var r: Double = 0;
			var g: Double = 0;
			if (Math.abs(q) <= split1)
				{
					//System.out.println("CASE 1" );
					r = const1 - q * q;
					g = q * poly(a, r) / poly(b, r);
				}
			else
				{
					//System.out.println("CASE 2" );
					if (q < 0)
						r = p
					else
						r = 1 - p

					if (r <= 0)
						{
							//System.out.println(" (a)");
							g = -1;
						}
					else
						{
							//System.out.println("  (b)");
							r = Math.sqrt(-Math.log(r));
							if (r <= split2)
								{
									//System.out.println("   (i)");
									r = r - const2
									g = poly(c, r) / poly(d, r)
								}
							else
								{
									//System.out.println("   (ii)");
									r = r - split2
									g = poly(e, r) / poly(f, r)
								}
							//r=r-split2
							if (q < 0)
								g = -g
						}
				}
			g
		}

	def poly(coeff: Array[Double], x: Double): Double =
		{
			var result: Double = 0;
			var i: Int = coeff.length - 1;
			while (i >= 0)
				{
					result = result * x + coeff(i);
					i -= 1;
				}
			result;
		}

	//broken don't use...
	def poly(coeff: List[Double], x: Double): Double =
		coeff.foldLeft(0.0)((result, v) => result * x + v)
	//    def worldTrueScore(vars:Iterable[Variable]) : Double = vars.foldLeft(0.0)((total,v) => total + v.trueScore)

	def logit(p: Double) = Math.log(p / (1 - p));

	def main(args: Array[String])
		{
			val lst = List(1.0, 2.0, 3.0);
			val arr = lst.toArray;
			System.out.println("POLY: " + poly(lst, 0.1));
			System.out.println("POLY: " + poly(lst, 1));
			System.out.println("POLY: " + poly(lst, 0.25));

			System.out.println("POLY: " + poly(a, 0.25));
			System.out.println("POLY: " + poly(a.toArray, 0.25));

			System.out.println("probit(0.025)=" + probit(0.025));
			System.out.println("probit(0.975)=" + probit(0.975));
			//System.out.println("erfn-1(0.975)="+erfInv(0.025));
			//System.out.println("erfn-1(0.975)="+erfInv(0.975));

			System.out.println("probit(0.05)=" + probit(0.05));
			System.out.println("probit(0.95)=" + probit(0.95));

			System.out.println("probit(0.75)=" + probit(0.75));
			System.out.println("probit(0.25)=" + probit(0.25));

			System.out.println("probit(0.5)=" + probit(0.5));
			System.out.println("probit(0.1337)=" + probit(0.1337));

		}
}

}
