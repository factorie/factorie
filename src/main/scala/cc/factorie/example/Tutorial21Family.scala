package cc.factorie.example

import cc.factorie._ 
import cc.factorie.la._ 

object Tutorial21Family {
  def main(args:Array[String]): Unit = {
    
    val family1 = new Family1[BooleanVariable] {
      def score(b:BooleanValue) = if (b.booleanValue) 1.0 else 0.0
    }

    val family2 = new DotFamilyWithStatistics2[BooleanVariable,BooleanVariable] {
      val weights = new DenseTensor2(Array(Array(1.0, 0.0), Array(0.0, 1.0)))
    }
    
  }

  
  // TODO Consider giving a Family to singleton Factors, to support learning on them. 
  
}
