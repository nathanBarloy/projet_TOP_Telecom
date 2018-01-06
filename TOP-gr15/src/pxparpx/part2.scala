package pxparpx
import Array._

object part2 {
  
  
  def pxParPxNB(img:Array[Array[Int]],motif:Array[Array[Int]]):List[Array[Int]]={
	/* Ce programme prend en paramètres une image et un motif de plus petite taille et vérifie que le motif appartient bien à l'image, puis retourne toutes les positions auwquelles le motif a été trouvé (position du pixel le plus en haut à gauche) */
	
	var wimg=img.length; //dimensions width et height de l'image et du motif
	var himg=img(0).length;
	var wmot=motif.length;
	var hmot=motif(0).length;
	
	var liste_trouves:List(Array(Int)) = Nil;
	
	if (wimg >= wmot && himg >= hmot){  //On vérifie que le motif est plus petit que l'image
		for(i <- 0 to himg-hmot){
			for(j<- 0 to wimg-wmot){  //Chaque sous-image de même taille quele motif est vérifié
				var k = 0;
				var l = 0;
			while(img(i+k)(j+l)==motif(k)(l)&&k<hmot){  //Tant que le motif concorde pixel par pixel, on avance dans les lignes et les rangées jusqu'au dernier pixel
					if(l<wmot-1){  //Si on n'est pas arrivé au bout d'une ligne, on avance dans la ligne
						l++;
					}else{      //Sinon on passe à la ligne suivante
						k++; 
						l=0;
					}
				}
				if(k==hmot){
					Array(i,j)::liste_trouves;
				}
			}
		}
	
	}
	return liste_trouves;
}
}