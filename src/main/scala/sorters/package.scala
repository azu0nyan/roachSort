package object sorters {


  /**
   * 
   * @param n elements total
   * @param k elements to pick
   * @return
   */
  def nkIterator(n:Int, k:Int):Iterator[Array[Int]] = new Iterator[Array[Int]]{
    
    var first = true
    val cur:Array[Int] =  (0 until k).toArray
    var hn = true
    
    def hasNext = hn 
    def next():Array[Int] = {
      if(first)
        first = false                
      else {
        hn = nextNK(cur, n)
      }
      cur
    }      
      
  }
  
   def nextNK(arr:Array[Int], n:Int):Boolean = {
    val k = arr.length
    var i = k - 1
    while( i >= 0) {
      if(arr(i) < n - k + i ){
        arr(i) +=1
        var j = i + 1
        while(j < k) {
          arr(j) = arr(j -1) + 1
          j += 1
        }
        return true
      }
      i -=1
    }
    return false
  }
}
