package pxparpx
import Array._

object part4 {       

   
  /************** création histogramme en niveaux de gris ***************/
  
  def histoNB(src:Array[Array[Int]]):Array[Int]={
    
    /*Ce programme réalise l'histogramme en niveaux de gris d'une image noir et blanc
     * 
     * L'histogramme est modelisé par un tableau 1D où la valeur de chaque index i correspond au nombre de pixel d'intensité i.*/
    
    
    var histo=Array.fill(256)(0) 
    var height = src.length
    var width = src(0).length
    
    for (row <-0 to height-1){
      for (col <-0 to width-1){
        var (t,r,g,b)=pxparpx.part1.convert(src(row)(col)) //On récupère les données du pixel
        histo(r) = histo(r)+1  //On utilise r,g ou b pour compléter le tableau (ils ont tous la même valeur)
      }
    }
    
    return histo
  }
  


}