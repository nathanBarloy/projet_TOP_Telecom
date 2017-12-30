

object Histoobj extends App {
   def convert(n:Int):(Int,Int,Int,Int)={ // Import de convert, greyLevel et ConvertBack de Nathan
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
      return (t*Math.pow(2,24).toInt+r*Math.pow(2,16).toInt+g*Math.pow(2,8).toInt+b)
  }
  def HistoNB(Src:Array[Array[Int]]):Array[Int]={ //Modelise un histogramme (Tableau 1D de taille 256) d'une image N&B 
    greyLevel(Src,0.299,0.587,0.114) //Convertie l'image d'entrée en image N&B
    var Histo=Array.fill(256)(0) //L'Histo est modelisé par un tableau 1D où la valeur de chaque index i correspond au nombre de pixel d'intensité i.
    var height = Src.length
    var width = Src(0).length
    for (row <-0 to height-1){
      for (col <-0 to width-1){
        var (t,r,g,b)=convert(Src(row)(col))  //Convertie chaque valeur de pixel en entier compris entre 0 et 255 ("Intensité du pixel")
        
        if (r<0){
          Histo(-r)=Histo(-r)+1 //Construit l'histo. Si la valeur est negative (Erreur dans le convert de Nathan), prend l'opposé.
        }
        else{
        Histo(r)= Histo(r)+1
        }
        
        
      }
    }
    
    
    for (i<-0 to Histo.length-1){
      println(Histo(i))
      
    }
    var (t,r,g,b)=convert(Src(450)(450))
    print(r)
    return Histo
  }
  
}

        
 