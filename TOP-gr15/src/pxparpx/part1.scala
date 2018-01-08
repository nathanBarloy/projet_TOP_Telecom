package pxparpx
import Array._

object part1 {
  def test():Unit={
    println("rien")
  }
  
  def greyLevel(src:Array[Array[Int]] , coeffR:Double=0.299 , coeffG:Double=0.587 , coeffB:Double=0.114):Unit={
    /* ce programme converti une image couleur en niveau de gris.
     * les coefficients de chaque couleur peuvent etre modifiés.
     */
    var height=src.length
    var width=src(0).length
    for (i<-0 to height-1) { 
      for (j<-0 to width-1) {
        var (t,r,g,b)=convert(src(i)(j))
        var misNiveau=(coeffR*r+coeffG*g+coeffB*b).toInt //pour chaque pixel, on fait la moyenne pondérée des trois couleurs rgb
        src(i)(j) = convertBack(t,misNiveau,misNiveau,misNiveau)
      }
    }
  }
  
  def convert(n:Int):(Int,Int,Int,Int)={
    /*converti l'entier représentant le pixel en un quadruplet (transparence,rouge,vert,bleu)
     * on fait pour cela du décalage de bits
     */
    var t = (n >> 24) & 0xFF
    var r = (n >> 16) & 0xFF
    var g = (n >> 8) & 0xFF
    var b = n & 0xFF
    return (t,r,g,b)
  }
  
  def convertBack(t:Int,r:Int,g:Int,b:Int):Int={
    //fait l'inverse de convert : part du quadruplet (t,r,g,b) pour donner l'entier correspondant
    return ((t << 24) + (r << 16) + (g << 8) + b)
  }
}