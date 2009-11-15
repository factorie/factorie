package cc.factorie

trait RealValue {
  this: Variable =>
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  def ===(other: RealValue) = doubleValue == other.doubleValue
  def !==(other: RealValue) = doubleValue != other.doubleValue
  override def toString = printName + "(" + doubleValue.toString + ")"
}

class RealObservation(val doubleValue:Double) extends Variable with RealValue

/**A variable class for real values. */
class RealVariable(initialValue: Double) extends Variable with RealValue {
  def this() = this(0.0)
  type VariableType = RealVariable
  private var _value: Double = initialValue
  @inline final def doubleValue = _value
  def +=(x:Double) = set(_value + x)(null)
  def -=(x:Double) = set(_value - x)(null)
  def *=(x:Double) = set(_value * x)(null)
  def /=(x:Double) = set(_value / x)(null)
  // TODO Consider implementing 'def update' for syntax like val x = RealVariable; x = 3 ???
  def set(newValue: Double)(implicit d: DiffList): Unit =
    if (newValue != _value) {
      if (d != null) d += new RealDiff(_value, newValue)
      _value = newValue
    }
  def :=(newValue:Double) = set(newValue)(null) // Go through 'set' so we can do coordination in subclasses.
  //@inline override def :=(x:Double) = _value = x // TODO Do we really want to preclude useful overrides to 'set'? 
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}

// Temporary alias to old name  // TODO Remove this?
class Real(v:Double) extends RealVariable(v)
