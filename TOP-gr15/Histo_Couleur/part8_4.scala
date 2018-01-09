package pxparpx
import Array._
import scala.annotation.switch

object part8_4 {
  
  def select(r:Int,g:Int,b:Int,couleur:String):Int={
    /* programme permettant de sélectionner quelle composante on veut étudier*/
    var c = 0
    if (couleur == "r"){
       c = r
    }else if (couleur == "g"){
       c = g
    }else if (couleur == "b"){
       c = b
    }
    
    return c
    
  }
  
    def histoC(src:Array[Array[Int]],couleur:String):Array[Int]={
    /*Ce programme réalise l'histogramme en l'une des trois couleurs d'une image couleur
     * 
     * L'histogramme est modelisé par un tableau 1D où la valeur de chaque index i correspond au nombre de pixel d'intensité i.*/
    
    
    var histo=Array.fill(256)(0) 
    var height = src.length
    var width = src(0).length
    
    
    for (row <-0 to height-1){
      for (col <-0 to width-1){
        var (t,r,g,b)=pxparpx.part1.convert(src(row)(col)) //On récupère les données du pixel
        
        var c = select(r,g,b,couleur)
        
        histo(c) = histo(c)+1  //On utilise r,g ou b pour compléter le tableau selon l'histogramme que l'on fait
      }
    }
    
    return histo
  }
  
}