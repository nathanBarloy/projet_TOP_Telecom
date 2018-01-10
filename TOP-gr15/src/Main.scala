import com.tncy.top.image.ImageWrapper;

object Main {
  def main(args:Array[String]):Unit={
    
    //pour appeler des fonctions d'autres fichiers :
    //pxparpx.part1.test();
    
    
    //mettre des choses ici
    def time[R](block: => R): R = {  
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + "ns")
        result
    }
    
    
    var wrappedImage : ImageWrapper = new ImageWrapper("test/test15/image.png")
    
    var image2D=wrappedImage.getImage()
    //pxparpx.part1.greyLevel(image2D)
    var wrappedMotif : ImageWrapper = new ImageWrapper("test/test15/motif.png")
    var motif2D=wrappedMotif.getImage()
    var liste=time{pxparpx.part8_5.rechercheHisto(image2D,motif2D)}
    pxparpx.resultat.imageAvecCarres(wrappedImage,wrappedMotif,liste,"test/test15/resultat.png")
    ////pxparpx.part1.greyLevel(motif2D)
    
    
    //pxparpx.part10.carteChaleur(wrappedImage,wrappedMotif,"test/test13/resultat.png")
    
    
   /* var wrappedImage : ImageWrapper = new ImageWrapper("test/test04/image.png")
    var image2D=wrappedImage.getImage()
    var wrappedMotif : ImageWrapper = new ImageWrapper("test/test04/motif.png")
    var motif2D=wrappedMotif.getImage()
    var hi = pxparpx.part6.histoIntegral(image2D)
    var he = pxparpx.part6.extractHistoIntegral(hi,1,5,motif2D)*/
    
    /*var h = pxparpx.part4.histoNB(image2D)
    println("part4 : ")
    println(h(255))
    println(h(0))
    println("part6 : ")
    println(hi(hi.length-1)(hi(0).length-1)(255))
    
    println(hi(hi.length-1)(hi(0).length-1)(0))*/
  }
}