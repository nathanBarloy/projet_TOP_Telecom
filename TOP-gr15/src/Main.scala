import com.tncy.top.image.ImageWrapper;

object Main {
  def main(args:Array[String]):Unit={
    
    //pour appeler des fonctions d'autres fichiers :
    //pxparpx.part1.test();
    
    
    //mettre des choses ici
    
    var wrappedImage : ImageWrapper = new ImageWrapper("image/logo_TNCY.png")
    var Image2D=wrappedImage.getImage()
    println(Image2D(0)(0))
    pxparpx.part1.greyLevel(Image2D,0.33,0.34,0.33)
    wrappedImage.saveImage("image/logo_greyLvl.png")
    
  }
}