package pxparpx
import Array._

object part5 {
  
  
  
  /******* Programme auxiliaire créant une sous-image *******/
  
  
  
  def sousImage(img:Array[Array[Int]], motif:Array[Array[Int]], i:Int, j:Int):Array[Array[Int]] ={
    
    /* Ce programme crée une sous-image de img de taille égale à motif 
     * à partir du pixel (i,j)
     */
    
    var height = motif.length
    var width = motif(0).length
    var sous_image = ofDim[Int](height,width)
    
    for(k<-0 to height-1){
      for(l <- 0 to width-1){
        sous_image(k)(l) = img(k+i)(l+j)  //copie pixel par pixel à partir de (i,j)
      }
    }
    
    return sous_image
  }
  
  
  
  /******* programme de comparaison d'histogrammes ******/
  
  
  
  def compareHisto(h1:Array[Int],h2:Array[Int]):Boolean ={
    
    
    /* ce programme teste si deux histogrammes sont égaux */
    
    
    var k = 0
    
    while(h1(k)==h2(k) && k<255) k += 1 //tant que chaque valeur est identique, on passe à la suivante
    
    return (k==255)  //vrai ssi tout l'histogramme a été parcouru
  }
  
  
  
  /******* programme principal de recherche par correspondance d'histogramme ********/
  
  
  
  def rechercheHisto(img:Array[Array[Int]],motif:Array[Array[Int]]):List[Array[Int]]={
    
    /* ce programme trouve toutes les correspondances d'un motif dans img par comparaison d'histogrammes */
    
    var resultat:List[Array[Int]] = List()
    var iheight=img.length
    var iwidth = img(0).length
    var mheight=motif.length
    var mwidth = motif(0).length
    
    var hmotif = pxparpx.part4.histoNB(motif) //histogramme du motif
    
    for (i<-0 to iheight-mheight){
      for (j<- 0 to iwidth-mwidth){
        var sous_img = sousImage(img,motif,i,j)  //on compare l'histogramme de la sous-image à l'emplacement (i,j)
        var himg = pxparpx.part4.histoNB(sous_img)  //On crée l'histogramme de la sous-image
        if(compareHisto(hmotif,himg)) resultat = Array(i,j)::resultat  //Si les histogrammes correspondent, on ajoute la position à la liste des correspondances
      }
    }
    return resultat
    
  }
  
}