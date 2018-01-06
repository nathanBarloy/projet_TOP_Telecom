package pxparpx
import Array._

object resultat {
  
  
	def imageAvecCarres(img:ImageWrapper, motif:ImageWrapper, liste_occurrences:List[Array[Int]], sortie:String){
		/*Cette fonction crée  l'image tirée de l'image d'entrée img avec des rectangles rouges aux dimensions du motif sur les emplacements de la liste des occurences*/
		var wmot=motif.width;
		var hmot=motif.height;	
		var new_image: Array[Array[Int]]= img.getImage();
		for(dim <- liste_occurrences){
			for(j <- 0 to wmot-1){								//On traite les bords supérieur et inférieur
				new_image(dim(0))(dim(1)+j)=0x00FF0000;
				new_image(dim(0)+hmot-1)(dim(1)+j)=0x00FF0000;
			}
			for(i <- 0 to hmot-1){								//on traite les bords gauche et droite
				new_image(dim(0)+i)(dim(1))=0x00FF0000;
				new_image(dim(0)+i)(dim(1)+wmot-1)=0x00FF0000;
			}
		}
		img.saveImage(sortie);
	}
}