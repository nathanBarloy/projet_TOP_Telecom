package pxparpx
import Array._

object part6 {

	def copy(h:Array[Int]):Array[Int] = {
		var m = h.length;
		var hcopie = ofDim[Int](m); //Tableau copié à renvoyer
		for (i <- 0 to m-1){
			hcopie(i)=h(i);
		}
		return hcopie;
	}
	
	
	def transfoTableGris(img:Array[Array[Int]]):Array[Array[Int]]={
	  var h=img.length
	  var w=img(0).length
	  var sortie = Array.fill(h,w)(0)
	  for (i <- 0 to h-1) {
	    for (j <- 0 to w-1){
	      sortie(i)(j)= img(i)(j) & 0xFF
	    }
	  }
	  return sortie
	}

	def histoIntegral(img:Array[Array[Int]]): Array[Array[Array[Int]]] = {
		
		/* ce programme génère un tableau dans lequel la case (i,j) contient l'histogramme de l'image rognée entre les points (0,0) (coin haut-gauche) et (i,j) (coin bas-droite) 
		Il prend en argument le tableau de valeurs de l'image à étudier et renvoie le tableau composé de chaque histogramme*/
		var imgGris = transfoTableGris(img)
		var m= imgGris.length;
		var n= imgGris(0).length;
		var resultat = Array.fill(m,n)(Array.fill(256)(0));
		
		
		resultat(0)(0)(imgGris(0)(0)) = 1;      //On initialise le premier histogramme 
		
		for(j <- 1 to n-1){                 //On  crée les histogrammes de la première rangée
			resultat(0)(j) = copy(resultat(0)(j-1));
			resultat(0)(j)(imgGris(0)(j)) = resultat(0)(j-1)(imgGris(0)(j-1)) + 1;
		}
		
		for (i <-1 to m-1){                 //On crée les histogrammes pour chaque rangée
			resultat(i)(0) = copy(resultat(i-1)(0));
			resultat(i)(0)(imgGris(i)(0)) = resultat(i)(0)(imgGris(i)(0)) + 1
			for(j <- 1 to n-1){
				var somme_aux = sommeHisto(resultat(i)(j-1),resultat(i-1)(j));  //On doit soustraire h(i-1,j-1) car cet ensemble est compté deux fois
				resultat(i)(j) = soustrHisto(somme_aux,resultat(i-1)(j-1))
				resultat(i)(j)(imgGris(i)(j)) = resultat(i)(j)(imgGris(i)(j)) + 1;
			}
		}
		return resultat
		
	}
	def sommeHisto(h1:Array[Int],h2:Array[Int]):Array[Int]={
		/*Ce programme effectue la somme terme à terme de deux histogrammes*/
		var somme = Array.fill(256)(0);
		if(h1.length == h2.length){		
			for (i <- 0 to 255){
				somme(i)=h1(i)+h2(i);
			}
		}
		return somme;
	}
	def soustrHisto(h1:Array[Int],h2:Array[Int]):Array[Int]={
		/*Ce programme effectue la soustraction terme à terme de deux histogrammes*/
		var soustr = Array.fill(256)(0);
		if(h1.length == h2.length){
			
			for (i <- 0 to 255){
				soustr(i)=h1(i)-h2(i);
			}
		}
		return soustr;
	}
	def extractHistoIntegral(hist_int: Array[Array[Array[Int]]], i:Int, j:Int, motif: Array[Array[Int]]):Array[Int]={
		/*Ce programme extrait d'un histogramme intégral associé à une image l'histogramme associé à la sous-image aux dimensions du motif à la position (i,j)
		
		Pour ce faire, on effectue des sommes et des soustractions sur les histogrammes liés aux quatre bords de l'histogramme intégral.*/
		var m = motif.length;
		var n = motif(0).length;
		var resultat = Array.fill(256)(0);
		if(i == 0 && j == 0){
		  resultat =  hist_int(m-1)(n-1)
		}else if(i ==0){
		  var ajout = hist_int(m-1)(n+j-1); //Histogrammes en somme positive dans le calcul : le "plus grand" et le "plus petit"
		  var substrat = hist_int(m-1)(j-1);  //Histogrammes en somme négative (soustraction) dans le calcul : les effets de bord
		  resultat = soustrHisto(ajout,substrat);
		}
		else if (j== 0){
		  var ajout = hist_int(m+i-1)(n-1); 
		  var substrat = hist_int(i-1)(n-1);
		  resultat = soustrHisto(ajout,substrat);  

		}else{
		  var ajout = sommeHisto(hist_int(i+m-1)(j+n-1),hist_int(i-1)(j-1)); //Histogrammes en somme positive dans le calcul : le "plus grand" et le "plus petit"
		  var substrat = sommeHisto(hist_int(i+m-1)(j-1),hist_int(i-1)(j+n-1));  //Histogrammes en somme négative (soustraction) dans le calcul : les effets de bord
      resultat = soustrHisto(ajout,substrat);
		}
		
		
		return resultat;
	}
}