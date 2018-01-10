package pxparpx
import Array._
import com.tncy.top.image.ImageWrapper;

object part10 {
  def compareHisto(h1:Array[Int],h2:Array[Int]):Int ={
    var k = 0
    for (i<-0 to 255){
      k = k + Math.abs(h2(i)-h1(i))
    }
    return k
  }
  
  def differenceHistoInt(img:Array[Array[Int]],motif:Array[Array[Int]]):Array[Array[Int]] = {
    
    
    var x = img.length
    var y = img(0).length
    var m = motif.length
    var n = motif(0).length
    var resultat:Array[Array[Int]] = ofDim[Int](x,y)
    
    var hmotifr = pxparpx.part8_4.histoC(motif,"r")
    var hist_intr = pxparpx.part8_6.histoIntegralC(img,"r")
    var hmotifg = pxparpx.part8_4.histoC(motif,"g")
    var hist_intg = pxparpx.part8_6.histoIntegralC(img,"g")
    var hmotifb = pxparpx.part8_4.histoC(motif,"b")
    var hist_intb = pxparpx.part8_6.histoIntegralC(img,"b")
    var himg = ofDim[Int](256)
    var difference = 0
    for (i<-0 to x-m){
      for (j<- 0 to y-n){
        
        himg = pxparpx.part6.extractHistoIntegral(hist_intr, i, j, m, n)
        difference = compareHisto(hmotifr,himg)
        himg = pxparpx.part6.extractHistoIntegral(hist_intg, i, j, m, n)
        difference += compareHisto(hmotifg,himg)
        himg = pxparpx.part6.extractHistoIntegral(hist_intb, i, j, m, n)
        difference += compareHisto(hmotifb,himg)
        
        
        resultat(i)(j) = difference
        
      }
    }
    return resultat
    
  }
  
  def max(tab:Array[Array[Int]]):Int={
    var m = tab.length
    var n = tab(0).length
    var res = 0
    for(i <- 0 to m-1){
      for(j <- 0 to n-1){
        if(tab(i)(j) > res) res = tab(i)(j)
      }
    }
    return res
  }
  
  
  def carteChaleur(img:ImageWrapper, motif:ImageWrapper,sortie:String){
    var tab_img = img.getImage()
    var tab_motif = motif.getImage()
    var differences = differenceHistoInt(tab_img,tab_motif)
    var diff_max = max(differences)
    println(diff_max)
    println(differences(100)(100))
    var coeff = 255.0/diff_max
    var m = tab_img.length
    var n = tab_img(0).length
    var tmp = 0.0
    for(i <- 0 to m-1){
      for(j<-0 to n-1){
        tmp=differences(i)(j)*coeff/2
        differences(i)(j) = tmp.toInt
        tab_img(i)(j) = pxparpx.part1.convertBack(255,255-differences(i)(j),0,differences(i)(j))
      }
    }
    img.saveImage(sortie); 
  }
  
}
