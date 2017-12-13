package pxparpx
import Array._

object part1 {
  def test():Unit={
    println("rien")
  }
  
  def greyLevel(src:Array[Array[Int]] , coeffR:Double=0.299 , coeffG:Double=0.587 , coeffB:Double=114):Array[Array[Int]]={
    var height=src.length
    var width=src(0).length
    var greylvl=ofDim[Int](height,width)
    for (i<-0 to height-1) {
      for (j<-0 to width-1) {
        var (t,r,g,b)=convert(src(i)(j))
        var misNiveau=(coeffR*r+coeffG*g+coeffB*b).toInt
        greylvl(i)(j) = convertBack(t,misNiveau,misNiveau,misNiveau)
      }
    }
    return greylvl
  }
  
  def convert(n:Int):(Int,Int,Int,Int)={
    var a=n
    var t=(a/Math.pow(2,24)).toInt
    a -= t*(Math.pow(2,24).toInt)
    var r=(a/Math.pow(2,16)).toInt
    a -= r*(Math.pow(2,16).toInt)
    var g=(a/Math.pow(2,8)).toInt
    a -= g*(Math.pow(2,8).toInt)
    var b=a
    return (t,r,g,b)
  }
  
  def convertBack(t:Int,r:Int,g:Int,b:Int):Int={
    return (t*Math.pow(2,24).toInt+r*Math.pow(2,16).toInt+g*Math.pow(2,8).toInt+b)
  }
}