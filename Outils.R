library("grid")
library("TurtleGraphics")



OutilsChunk<-function(posX,posY,Longueur,Hauteur)
{
  setpos(posX,posY+40+30)
  Quadri(Longueur,10,0.5,"#2C833D",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur+40+30,0.5,"#8A502F",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur+10+30,0.5,"#A2A5A4",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur-2+30,0.5,"#8C8D8D",7)
}

Quadri <- function(largeur,hauteur,i,colour,epaisseur) 
{
  k <- largeur
  l <- hauteur
  turtle_param(colour,lwd = epaisseur)
  
  turtle_right(90)
  turtle_forward(k/2)
  turtle_backward(k/2)
  turtle_left(90)
  turtle_forward(l)
  turtle_right(90)
  turtle_forward(k)
  turtle_right(90)
  turtle_forward(l)
  turtle_right(90)
  turtle_forward(k-i)
  turtle_right(90)
  
  while(k > 0 && l > 0)
  {
    l <- l-i
    #○print(l)
    turtle_forward(l)
    turtle_right(90)
    #Sys.sleep(0.5)
    k <- k-i
    #print(k)
    turtle_forward(k)
    turtle_right(90)
    #Sys.sleep(0.5)
  }
}

setpos<-function(x,y)
{
  turtle_up()
  turtle_setpos(x,y)
  turtle_down()
  turtle_setangle(0)
}

SommeVecteur <- function(vecteur,taille_vecteur) # Somme des i+2
{
  # print(vecteur)
  i<-1
  somme <- 0
  while (i < taille_vecteur)
  {
    somme <- somme + vecteur[i]
    i <- i+2
  }
  return(somme)
}
