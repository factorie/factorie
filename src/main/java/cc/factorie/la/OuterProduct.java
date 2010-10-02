package cc.factorie.la;

class OuterProduct {

  public static int[] computeMatrix(int[] a, int[] b, int bsize) {
    int alen = a.length, blen = b.length;
    int[] arr = new int[alen * blen];
    int i = 0; 
    for (int i1=0; i1<alen; i1++) {
      for (int i2=0; i2<blen; i2++) {
        arr[i] = a[i1] * bsize + b[i2];
        i += 1;
      }
    }
    return arr;
  }
}