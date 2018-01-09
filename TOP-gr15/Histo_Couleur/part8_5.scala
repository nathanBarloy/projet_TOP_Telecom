package pxparpx
import Array._
 
object part8_5 {
  
  def sousImage(img:Array[Array[Int]], motif:Array[Array[Int]], i:Int, j:Int):Array[Array[Int]] ={
    /*crée une image de taille égale au motif à partir du pixel (i,j) de l'image de base*/
    
    var height = motif.length
    var width = motif(0).length
    var sous_image = ofDim[Int](height,width)
    for(k<-0 to height-1){
      for(l <- 0 to width-1){
        sous_image(k)(l) = img(k+i)(l+j)
      }
    }
    return sous_image
  }
  
  def compareHisto(h1:Array[Int],h2:Array[Int]):Boolean ={
    /* test primaire de comparaison d'histogrammes*/
    var k = 0
    while(h1(k)==h2(k) && k<255) k += 1  //Tant que l'on a égalité et qu'on n'a pas atteint la fin de l'histogramme, on incrémente k
    return (k==255)  //vrai uniquement si toutes les valeurs sont égales
  }
  
  def rechercheHisto(img:Array[Array[Int]],motif:Array[Array[Int]]):List[Array[Int]]={
    var resultat:List[Array[Int]] = List()
    var iheight=img.length
    var iwidth = img(0).length
    var mheight=motif.length
    var mwidth = motif(0).length
    var hmotifr = pxparpx.part8_4.histoC(motif,"r")
    var hmotifg = pxparpx.part8_4.histoC(motif,"g")
    var hmotifb = pxparpx.part8_4.histoC(motif,"b")
    for (i<-0 to iheight-mheight){
      for (j<- 0 to iwidth-mwidth){
        var sous_img = sousImage(img,motif,i,j)
        var himgr = pxparpx.part8_4.histoC(sous_img,"r")
        var himgg = pxparpx.part8_4.histoC(sous_img,"g")
        var himgb = pxparpx.part8_4.histoC(sous_img,"b")
        var identiques = compareHisto(hmotifr,himgr) && compareHisto(hmotifg,himgg) && compareHisto(hmotifb,himgb)
        if(identiques) resultat = Array(i,j)::resultat
      }
    }
    return resultat
    
  }
}