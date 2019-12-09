library("grid")
library("TurtleGraphics")

#############################################################################
################### DESSIN DU CHUNK #########################################
#############################################################################

OutilsChunk<-function(posX,posY,Longueur,Hauteur)
{
  # On dessine successivement les diferentes couches de chunks,
  # pour donner du relief.
  
  setpos(posX,posY+40+30)
  Quadri(Longueur,10,0.5,"#2C833D",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur+40+30,0.5,"#8A502F",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur+10+30,0.5,"#A2A5A4",7)
  setpos(posX,0)
  Quadri(Longueur,Hauteur-2+30,0.5,"#8C8D8D",7)
}

#############################################################################
#############################################################################


#############################################################################
###################  Dessin Arbre   #########################################
#############################################################################

OutilsArbre <- function(posX,posY,taille_monde,vecteur,taille_vecteur)
{
  # Tous les chiffres qui suivent " 2/11 ... + 85 ... etc sont trouves a tatonement.
  # Ils n'ont pas une signification particulieres
  
  # On commence par faire dessiner les troncs par couche pour donner du details.
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille_monde),((2/11)*taille_monde),0.5,"brown",7)
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille_monde),((2/11)*taille_monde),0.5,"black",4)
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille_monde),((2/11)*taille_monde),0.5,"brown",2)
  
  setpos(posX+2,posY+85)
  Quadri(((2/87)*taille_monde),((2/11)*taille_monde),0.5,"brown",0.8)
  
  setpos(posX,posY+85+((2/11)*taille_monde)) # On deplace la tortue en haut de l'arbre
                                             # avant de commencer le feuillage. 
  # une fois le tronc dessine, on appel la fonction qui va dessiner les feuilles.
  
  OutilsFeuillage(vecteur,taille_monde,taille_vecteur)
}

OutilsFeuillage<-function(vecteur,taille_monde,taille_vecteur)
{
  turtle_setangle(0)
  turtle_forward(taille_monde*(2/20)/2) # on deplace la tortue au dessus du tronc,
                                        # et on fait un premier feuillage central.
  for(i in 1:taille_vecteur)
  {
    # On commence par un Oursin vert fonce, puis plus petit un Oursin vert clair.
    
    couleur <- "#1E3605" #Vert fonce
    
    taille <- vecteur[i]*6
    turtle_param(lwd=6)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
    
    couleur <- "#2E4219" #Vert clair
    
    taille <- vecteur[i]*3
    turtle_param(lwd=6)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/taille_vecteur)
  }
  turtle_backward(taille_monde*(2/20)/2) # on revient a la position initiale.
  
  turtle_right(60)
  
  for(j in 1:6) # On trace un hexagone et on place a chaque sommet un feuillage (Oursin)
  {
    for(k in 1:taille_vecteur)
    {
      # On commence par un Oursin vert fonce, puis plus petit un Oursin vert clair.
      
      couleur <- "#1E3605" #Vert fonce
      
      taille <- vecteur[k]*6
      turtle_param(lwd=6)
      turtle_col(couleur)
      turtle_forward(taille)
      turtle_backward(taille)
      turtle_right(360/taille_vecteur)
      
      couleur <- "#2E4219" #Vert clair
      
      taille <- vecteur[k]*3
      turtle_param(lwd=6)
      turtle_col(couleur)
      turtle_forward(taille)
      turtle_backward(taille)
      turtle_right(360/taille_vecteur)
    }
    # Une fois le feuillage termine on continue notre hexagone, et on place un nouveau feuillage,
    # On repete cette action 6 fois.
    
    turtle_up()
    turtle_forward(taille_monde*(2/35))
    turtle_down()
    turtle_left(60)
  }
}

#############################################################################
#############################################################################

#############################################################################
###################  Dessin Buisson   #######################################
#############################################################################

OutilsBuisson <- function(posX,posY,vecteur,taille_vecteur)
{
  # Le buisson se compose de la même maniere que les feuillages des arbres.
  # On commence part un oursin Vert fonce et un plus petit vert clair.
  # On repete cette action deux fois.
  
  setpos(posX, posY+120)
  OursinBuisson(vecteur,taille_vecteur)
  
  setpos(posX, posY+90)
  OursinBuisson(vecteur,taille_vecteur)
}

OursinBuisson<-function(vecteur,taille_vecteur)
{
  for(i in 1:taille_vecteur)
  {
    couleur <- "#2F5508" #Vert fonce
    
    taille <- vecteur[i]*6
    turtle_param(lwd=7)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/taille_vecteur)
    
    couleur <- "#54970D" #Vert clair
    
    taille <- vecteur[i]*3
    turtle_param(lwd=7)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/taille_vecteur)
  }
}

#############################################################################
#############################################################################

#############################################################################
###################  Outils de Dessin   #####################################
#############################################################################

Quadri <- function(largeur,hauteur,i,colour,epaisseur) 
{
  # Cette fonction permet de dessiner un quadrilatere et de le remplir.
  
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
  
  while(k > 0 && l > 0) # Tant que le quadrilatere n'est pas bien remplit on continue
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

OutilsAstre<-function(posX,posY,taille_monde)
{
  setpos(posX,posY)
  j<-0
  if(taille_monde>=950)
  {
    couleur="#FFD644"  # Couleur Soleil
    couleur2="#FFD644"  # Rayon de Soleil
    turtle_setangle(5)  ## On reangle la tortue pour ne pas faire de rayon sur l'horizon
    turtle_col(couleur2)
    turtle_param(lwd=1)
    while(j<360)
    {
      turtle_forward(taille_monde/5)
      turtle_backward(taille_monde/5)
      turtle_setangle(j)
      j<-j+28
    }
  }
  else
  {
    couleur="#808080"  ## Couleur Lune
  }
  turtle_col(couleur)
  turtle_param(lwd=7)
 # setpos(posX,posY)
 
  for(i in 1:360)    ## on fait un rond complet, meme si on n'en verra qu'une partie. On se garde simplement la possibilite de faire apparaitre un astre complet
  {
    turtle_setangle(i)
    turtle_forward(taille_monde/10)
    turtle_backward(taille_monde/10)
    
  }

}










setpos<-function(x,y) # Cette fonction est utile,
{                     # pour deplacer la tortue sans faire de trait.
  turtle_up()
  turtle_setpos(x,y)
  turtle_down()
  turtle_setangle(0)
}

#############################################################################
#############################################################################

SommeVecteur <- function(vecteur,taille_vecteur) # Permet d'obtenir la somme des tailles des chunks.
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
