package pxparpx
import Array._

object part8_7 {
  def compareHisto(h1:Array[Int],h2:Array[Int]):Boolean ={
    var k = 0
    while(h1(k)==h2(k) && k<255) k += 1
    return (k==255)
  }
  
  def rechercheHistoInt(img:Array[Array[Int]],motif:Array[Array[Int]]):List[Array[Int]]={
    var resultat:List[Array[Int]] = List()
    
    var x = img.length
    var y = img(0).length
    var m = motif.length
    var n = motif(0).length
    
    var hmotifr = pxparpx.part8_4.histoC(motif,"r")
    var hist_intr = pxparpx.part8_6.histoIntegralC(img,"r")
    var hmotifg = pxparpx.part8_4.histoC(motif,"g")
    var hist_intg = pxparpx.part8_6.histoIntegralC(img,"g")
    var hmotifb = pxparpx.part8_4.histoC(motif,"b")
    var hist_intb = pxparpx.part8_6.histoIntegralC(img,"b")
    var himg = ofDim[Int](256)
    var identiques = true
    for (i<-0 to x-m){
      for (j<- 0 to y-n){
        
        himg = pxparpx.part6.extractHistoIntegral(hist_intr, i, j, m, n)
        identiques = compareHisto(hmotifr,himg)
        himg = pxparpx.part6.extractHistoIntegral(hist_intg, i, j, m, n)
        identiques = identiques && compareHisto(hmotifg,himg)
        himg = pxparpx.part6.extractHistoIntegral(hist_intb, i, j, m, n)
        identiques = identiques && compareHisto(hmotifb,himg)
        
        
        if(identiques) resultat = Array(i,j)::resultat
      }
    }
    return resultat
    
  }
}