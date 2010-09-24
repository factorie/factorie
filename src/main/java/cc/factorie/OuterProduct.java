package cc.factorie;

import java.util.Iterator;

class OuterProduct {

  public static int[] computeMatrix(int[] a1, int[] a2, int a2rowsize) {
    int[] arr = new int[a1.length * a2.length];
    
    int i = 0; 
    for (int i1=0; i1<a1.length; i1++) {
      for (int i2=0; i2<a2.length; i2++) {
        arr[i] = a1[i1] * a2rowsize + a2[i2];
        i += 1;
      }
    }
    return arr;
  }

}