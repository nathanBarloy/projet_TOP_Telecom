package pxparpx
import Array._

object part7 {
  
  
  /****** programme testant si deux histogrammes sont identiques ****/
  
  def compareHisto(h1:Array[Int],h2:Array[Int]):Boolean ={
    var k = 0
    while(h1(k)==h2(k) && k<255) k += 1
    return (k==255)
    
  }
  
  
  
  /********* programme principal de recherche par histogramme *********/
  
  def rechercheHistoInt(img:Array[Array[Int]],motif:Array[Array[Int]]):List[Array[Int]]={
    var resultat:List[Array[Int]] = List()
    var x = img.length
    var y = img(0).length
    var m = motif.length
    var n = motif(0).length
    var hmotif = pxparpx.part4.histoNB(motif)
    var hist_int = pxparpx.part6.histoIntegral(img)
    for (i<-0 to x-m){
      for (j<- 0 to y-n){
        
        var himg = pxparpx.part6.extractHistoIntegral(hist_int, i, j, m, n)
        if(compareHisto(hmotif,himg)) resultat = Array(i,j)::resultat
      }
    }
    return resultat
    
  }
}