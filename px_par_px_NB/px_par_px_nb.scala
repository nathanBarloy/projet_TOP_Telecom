import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage 

def pxParPxNB(img:BufferedImage,motif:BufferedImage):Boolean={
	var wimg=img.getwidth;
	var himg=img.getHeight;
	var wmot=motif.getWidth;
	var hmot=motif.getHeight;
	if (wimg >= wmot && himg >= hmot){
		for(i <- 0 to himg-hmot){
			for(j<- 0 to wimg-wmot){
				var k = 0;
				var l = 0;
				while(img.getRGB(i+k,j+l)==motif.getRGB(k,l)&&k<hmot){
					if(l<wmot-1){
						l++;
					}else{
						k++;
						l=0;
					}
				}
				if(k==hmot){
					return true
				}
			}
		}
	
	}
	return false
}