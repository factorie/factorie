package cc.factorie;

import java.util.Iterator;

class OuterProduct {

  public static int[] computeMatrix(int[] a1, int[] a2, int a2rowsize) {
    int a1len = a1.length;
    int a2len = a2.length;
    int[] arr = new int[a1len * a2len];
    
    int i = 0; 
    for (int i1=0; i1<a1len; i1++) {
      for (int i2=0; i2<a2len; i2++) {
        arr[i] = a1[i1] * a2rowsize + a2[i2];
        i += 1;
      }
    }
    return arr;
  }
}