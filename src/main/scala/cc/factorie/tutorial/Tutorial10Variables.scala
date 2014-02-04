/*&
 * Variables Tutorial
 * =================
 *
 * Examples of FACTORIE Variables, which hold data values.
 **/

package cc.factorie.tutorial

import org.junit.Assert._
import cc.factorie._
import cc.factorie.variable._

object TutorialVariables {
  def main(args:Array[String]): Unit = {
    
    /*&
     * A Variable holds a value (data)
     * They hold a single value.
     * They do not hold distributions over values.
     *  A distribution over variable values are the result of inference, and are stored in a Marginal
     *  (many of which may be stored in a Summary).
     **/

    // Create a variable that holds an integer value
    val i = new IntegerVariable(0)
    println("Variable i has value "+i.value)
    assertEquals(i.value, 0)

    // Set its value to 2
    i := 2 // TODO Make this use an implicit DiffList defaulting to null.
    println("After i := 2, variable i has value "+i.value)
    assertEquals(i.value, 2)
    
    // === tests equality of Variable values.  == tests Variable object identity
    val j = new IntegerVariable(2)
    if (i === j) println("Variables i and j have equal values.") else println("Variables i and j have unequal values.")
    if (i == j) println("i and j are the same Variable.") else println("i and j are not the same Variable")
    assert(i === j)
    assert(!(i == j))

    // The Scala type of a variable's value is represented by the variable class' inner type `Value`....


    // We can also track changes with a DiffList and undo them. // TODO Move this material about DiffLists to a separate Tutorial file.
    val d = new DiffList
    i.set(3)(d) // This method will create a Diff object and append it to the DiffList d.
    println("After i.set(2), variable i has value "+i.value)
    assertEquals(3, i.value)
    d.undo()
    println("After DiffList.undo, variable i has value "+i.value)
    assertEquals(2, i.value)
    // A Diff and a DiffList can be re-done also
    d.redo()
    println("After DiffList.redo, variable i has value "+i.value)
    assertEquals(3, i.value)
    
    // Variables can be sub-classed, and often are in order to represent relations among data
    class MyIntegerVariable(initialValue:Int, val partner:IntegerVariable) extends IntegerVariable(initialValue)
    val k = new MyIntegerVariable(3, i)
    println("k's partner is "+k.partner)
    assert(k.partner == i)
    
    /*&
     * All Variables also have a Domain, which indicates something about the range of values.
     * Many of the Domain objects (such as IntegerDomain) don't have much functionality.
     * Important functionality is performed by DiscreteDomain and CategoricalDomain, which will be described below.
     **/
    val id: IntegerDomain = i.domain
    assert(i.domain == k.domain)

    /*&
     * There are many types of Variables holding different types of values.
     **/
    
    // Variable whose value is a String
    val s = new StringVariable("Hello") 
    // Variable whose value is a floating-point number
    val x = new DoubleVariable(3.14)
    
    // Some values are pointers to Scala objects
    
    // Variable whose value is a Scala pointer
    val r = new RefVariable(i)
    r := j
    // Variable whose value is a Tuple2 of Scala pointers
    val e = new EdgeVariable(i,j)
    // Variable whose value is a Tuple2 of Scala pointers, but you can only change the second of the pair
    val a = new ArrowVariable(i,j)
    // Variable whose value is a Set of Scala objects
    val set = new SetVariable[IntegerVariable]
    set += i
    set += j
    if (set.contains(i)) println("SetVariable s contains Variable i")
    assert(set.contains(i))
    // Variable whose value is a Seq of Scala objects
    val list = new SeqVariable(Seq(i, j))
    println("The first element of list is "+list.head)
    assert(i == list.head)
    
    /*&
     * Variables are defined in a deep hierarchy of type inheritance.
     * cc.factorie.Variable is the root of all variable classes.
     **/

    // cc.factorie.Var* are traits that capture various abstractions.
    val mv: MutableVar = s // All MutableVar have a set() method and a := method.
    //val cv: VarWithConstantValue = s // This would cause a compilation error.
    
    // cc.factorie.Variable* are classes, all of which are mutable
    // They have have Var* super types, many of which do not commit to mutability or to a particular storage mechanism for their value
    val sv: StringVar = s
    val msv: MutableStringVar = s
    /*&
     * So if you want, you can create your own subclass of StringVar 
     *  which stores its String value as an integer index into an external table of Strings, and has immutable value.
     * If your code needs a Variable holding a String value, but doesn't care about its mutability or representation,
     *  you can have more flexibility by specifying type StringVar rather than StringVariable.
     **/
    
    /*&
     * One important type of value is a Tensor.
     * There are many subclasses of Tensor in order to efficiently support various special cases
     **/
    import cc.factorie.la._
    val dt = new DenseTensor1(5) // Creates a vector of length 5, internally stored as an Array[Double]
    dt(0) = 1.2
    println("Tensor dt is "+dt)
    val st = new SparseTensor1(999999) // Creates a vector of length 999999, but which efficiently stores only its non-zero values
    st(555) = 2.3
    println("Tensor st oneNorm is "+st.oneNorm)
    assertEquals(st.oneNorm, 2.3, 0.01)
    val ut = new UniformTensor1(33, 0.1) // A vector of length 33, in which all values are 0.1
    // WeightsMap of ranks 1 through 4 are available
    val dt4 = new DenseTensor4(3, 4, 5, 2) // A Tensor with 4 dimensions, storing 3*4*5*2 numbers.
    println("dt4 value at indices (2,3,4,1) is "+dt4(2,3,4,1))

    /*&
     * WeightsMap
     * -------
     *
     * WeightsMap have many useful methods, including:
     *
     *   - dot (products)
     *   - outer (products)
     *   - twoNorm
     *   - etc.
     *
     * See Tutorial*WeightsMap for more information
     *
     * WeightsMap are important because some important special cases of Factors require variables with Tensor values
     *  because the Factors calculate their scores as dot-products between these Tensor values and a "weight" parameter Tensor.
     *
     * RealVariable is like DoubleVariable in that it holds a single floating-point number,
     *  however its value is a Tensor rather than a Double.  
     *
     * In particular it is a RealValue which inherits from ScalarTensor, which is a Tensor1 of length 1.
     **/
    val scalar = new RealValue(3.4) // A Tensor1 of length 1, holding value 3.4 at index 0
    val rv = new RealVariable(3.4)
    val rvv:RealValue = rv.value
    // TODO Tensor.equals should test equality of all values in the Tensor, but it does not yet. 
    
    // A widely used type of Tensor is SingletonBinaryTensor1, which is a "one-hot" Tensor with value 1.0 in its one-hot position.
    val sbt = new SingletonBinaryTensor1(999, 3) // A vector of length 999, with all zeros, except a 1.0 at index 3.
    println("Tensor sbt is "+sbt)
    
    /*&
     * A DiscreteVar holds one of a finite N values, each associated with an integer 0 through N-1.
     *  Its value is a DiscreteValue.
     * A CategoricalVar inherits from DiscreteVar, but each value is also associated with "category", usually a String.
     *  Its value is a CategoricalValue.
     * DiscreteValue and CategoricalValue inherit from SingletonBinaryTensor1.  They are a binary one-hot Tensor1.
     **/
    
    // The value N is the size of a DiscreteVariable's domain, and is stored in a DiscreteDomain
    val dd = new DiscreteDomain(10)
    // Unlike all the Variables discussed below, DiscreteVariable does not have its "domain" method pre-defined; users must define it.
    class MyDiscrete(v:Int) extends DiscreteVariable(v) { def domain = dd }
    val md = new MyDiscrete(4) // A DiscreteVariable, initialized to a one-hot vector with 1.0 at index 4.
    println("md value is "+md.value)
    println("md integer value is "+md.intValue)
    assertEquals(4, md.intValue)
    println("md domain size is "+md.domain.size)
    assertEquals(md.domain.size, 10)
    
    // CategoricalDomain[A] has type parameter A indicating the type of the category.
    // CategoricalDomain can be initialized with a list of such categories,
    //  or it can grow dynamically as values for new categories are requested.
    val cd = new CategoricalDomain[String]
    println("The index of category 'apple' is "+cd.index("apple"))
    assertEquals(0, cd.index("apple"))
    println("The index of category 'pear'  is "+cd.index("pear"))
    assertEquals(1, cd.index("pear"))
    println("The index of category 'kiwi'  is "+cd.index("kiwi"))
    assertEquals(2, cd.index("kiwi"))
    println("The index of category 'apple' is "+cd.index("apple"))
    assertEquals(0, cd.index("apple"))
    println("The category of index 2 is "+cd.category(2))
    assertEquals("kiwi", cd.category(2))
    // Clearly, CategoricalDomains are useful for mapping from Strings to integers and back, and heavily used in NLP.
    // They are also typically used for class label in tasks such as document classification.
    
    // We can make a CategoricalVariable with this domain.
    // Similarly to DiscreteVariable, we must make a subclass of CategoricalVariable in which we specify the domain.
    class MyCategorical(s:String) extends CategoricalVariable(s) { def domain = cd }
    val cv1 = new MyCategorical("peach")
    val cv2 = new MyCategorical("plum")
    println("cv1 value is "+cv1.value)
    println("cv1 category value is "+cv1.categoryValue)
    assertEquals("peach", cv1.categoryValue)
    println("cv1 integer value is "+cv1.intValue)
    assertEquals(cv1.intValue, cd.index("peach"))
    // The domain grew automatically to accomodate the new category values
    println("The cd CategoricalDomain size is now "+cd.size)
    assertEquals(5, cd.size)
    // You cannot create CategoricalValue or DiscreteValue yourself.  They are only created automatically inside their Domains.
    
    // BooleanVar is a subtype of CategoricalVar whose category is of type Boolean.
    // Its value is a BooleanValue, which inherits from CategoricalValue[Boolean], which in turn is a one-hot Tensor.
    val bv = new BooleanVariable(true)
    println("bv value is "+bv.value)
    println("bv boolean value is "+bv.booleanValue)
    println("bv integer value is "+bv.intValue)
    
    // TODO FeatureVectors
    val fvd = new CategoricalVectorDomain[String] { def dimensionsDomain = cd }
    class MyFeatureVector extends BinaryFeatureVectorVariable[String] { def domain = fvd }  

    /*&
     * Another type of Tensor is Masses.
     * These are WeightsMap with only non-negative elements, whose sum is efficiently cached.
     * They are useful for the parameters of a Dirichlet, and also as the sufficient statistics for a Proportions
     **/
    val m1 = new DenseMasses1(5) // A vector of length 5
    m1 := Array(.5, .5, .5, .5, .5)
    println("m1 sum is "+m1.sum)
    assertEquals(2.5, m1.sum, 0.01)
    
    /*&
     * Proportions inherits from Masses
     * They are WeightsMap with only non-negative elements, whose sum must always be 1.0.
     * Proportions also have a method "masses".  In some classes this returns itself.  
     *  In others it returns an inner object that holds the sufficient statistics determining the Proportions values.
     * Proportions themselves cannot be incremented (e.g. to gather counts for estimating a Proportions), but its inner Masses sometimes can.
     **/
    val dp = new DenseProportions1(3)
    dp.masses(0) += 3
    dp.masses(2) += 2
    println("dp masses are "+dp.masses)
    println("dp sum is "+dp.sum)
    assertEquals(dp.sum, 1.0, 0.001)
    println("dp proportions are "+dp)
    

    // All Variables that hold Tensor values inherit from TensorVar.
    // Its concrete, simplest subclass is TensorVariable, which can hold arbitrary WeightsMap.
    val tv = new TensorVariable(new DenseTensor2(Array(Array(2.0,3.0,4.0), Array(6.0,7.0,8.0))))
    
    

    /*&
     * Value types
     * -----------
     * 
     * The type of a Variable's value is available as a member type "Value"
     **/

    // For example, StringVariable#Value is String
    def printValue(v:StringVariable#Value): Unit = println(v)
    printValue("Hi there.")

    /*&
     * In the Variable trait, the Value type is not set but only "bounded", so that it can be refined in subclasses
     * i.e. trait Variable { type Value <: Any }
     * Some Var* traits bound the value, others set it.
     * For example
     * 
     *  - ``DiscreteVar[A<:DiscreteValue]`` bounds it, but ``DiscreteVariable[A<:DiscreteValue]`` sets it.
     *  - ``CategoricalVar[A<:CategoricalValue[C],C]`` bounds it, but ``CategoricalVariable[A<:CategoricalValue[C],C]]`` sets it.
     *  - ``MutableVar[A<:Any]`` sets it.
     * 
     * Whenever we define method that expects a parameter of type Variable#Value, the Value must be set, not merely bounded.
     * 
     * This is why MutableVar[] (which defines ``set(v:Value):Unit`` sets ``Value``, and why all *Variable classes have the Value type set.
     * 
     * The standard way to bound the Value type is to inherit from ValueBound[A], which bounds the Value <: A.
     * The standard way to set the Value type is to inherit from the trait Var[A], which sets the Value to A.
     **/

    
    // Assignment objects
    
    // While a Variable objects holds a value, values for variable may also be stored in an Assignment.
    val as = new HashMapAssignment
    as.update[IntegerVariable](i, 55)
    as.update[IntegerVariable](j,  66)
    // This allows some code to consider different values for a variable which changing the "global" value stored in the variable.
    //  (Helpful for multi-threaded code, among other reasons.)
    
  }

}
