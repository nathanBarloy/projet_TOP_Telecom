package pxparpx
import Array._

object part1 {
  def test():Unit={
    println("rien")
  }
  
  def greyLevel(src:Array[Array[Int]] , coeffR:Double=0.299 , coeffG:Double=0.587 , coeffB:Double=0.114):Unit={
    var height=src.length
    var width=src(0).length
    for (i<-0 to height-1) {
      for (j<-0 to width-1) {
        var (t,r,g,b)=convert(src(i)(j))
        var misNiveau=(coeffR*r+coeffG*g+coeffB*b).toInt
        src(i)(j) = convertBack(t,misNiveau,misNiveau,misNiveau)
      }
    }
  }
  
  def convert(n:Int):(Int,Int,Int,Int)={
  
    var t = (n >> 24) & 0xFF
    var r = (n >> 16) & 0xFF
    var g = (n >> 8) & 0xFF
    var b = n & 0xFF
    return (t,r,g,b)
  }
  
  def convertBack(t:Int,r:Int,g:Int,b:Int):Int={
    return ((t << 24) + (r << 16) + (g << 8) + b)
  }
}