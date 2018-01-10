
import Array._
object Histoobj extends App {
   def convert(n:Int):(Int,Int,Int,Int)={
    /*converti l'entier repr�sentant le pixel en un quadruplet (transparence,rouge,vert,bleu)
     * on fait pour cela du d�calage de bits
     */
    var t = (n >> 24) & 0xFF
    var r = (n >> 16) & 0xFF
    var g = (n >> 8) & 0xFF
    var b = n & 0xFF
    return (t,r,g,b)
  }

      
    
   def greyLevel(src:Array[Array[Int]] , coeffR:Double=0.299 , coeffG:Double=0.587 , coeffB:Double=0.114)={
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
   def convertBack(t:Int,r:Int,g:Int,b:Int):Int={
    //fait l'inverse de convert : part du quadruplet (t,r,g,b) pour donner l'entier correspondant
    return ((t << 24) + (r << 16) + (g << 8) + b)
  }

   def transfoTableGris(img:Array[Array[Int]]):Array[Array[Int]]={
    
	  /* Ce programme permet de récupérer, dans une image en niveaux de gris,
	   * un tableau composé de la valeur du gris de chaque pixel de 0 à 255.
	   */
    
    var h=img.length
	  var w=img(0).length
	  
	  var sortie = Array.fill(h,w)(0)
	  for (i <- 0 to h-1) {
	    for (j <- 0 to w-1){
	      sortie(i)(j)= img(i)(j) & 0xFF //on récupère uniquement le dernier octet
	    }
	  }
	  return sortie
	}

def histoNB(src:Array[Array[Int]]):Array[Int]={
    
    /*Ce programme réalise l'histogramme en niveaux de gris d'une image noir et blanc
     * 
     * L'histogramme est modelisé par un tableau 1D où la valeur de chaque index i correspond au nombre de pixel d'intensité i.*/
    
    
    var histo=Array.fill(256)(0) 
    var height = src.length
    var width = src(0).length
    
    for (row <-0 to height-1){
      for (col <-0 to width-1){
        var (t,r,g,b)=convert(src(row)(col)) //On récupère les données du pixel
        histo(r) = histo(r)+1  //On utilise r,g ou b pour compléter le tableau (ils ont tous la même valeur)
      }
    }
//    for (i<-0 to histo.length-1){
//      println(histo(i))
//      
//    }
    return histo
  }
    
    
}
    
  
  


        
 