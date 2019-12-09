library("grid")
library("TurtleGraphics")
source("Outils.R")


DoIt_Dessin <- function(vecteur,taille_vecteur)  # Fonction principale qui dessine l'integralite du dessin final
{
  print(vecteur) # Verification de l'integrite du seed :D
  taille_monde <- (SommeVecteur(vecteur,taille_vecteur) * 10)
  print(taille_monde) # debug
  Intitialisation(taille_monde)
  Ciel(taille_monde)
  Astre(taille_monde,vecteur,taille_vecteur)
  Chunk(vecteur,taille_vecteur)
  Arbre(vecteur,taille_monde,taille_vecteur)
  Buisson(vecteur,taille_monde,taille_vecteur)
  
  
}
  
Intitialisation <- function(taille_monde)
{
  turtle_init(taille_monde, (taille_monde*3/4), c("clip"))
  turtle_hide()
}

Ciel <- function(taille_monde)
{
  taille_mondeY=taille_monde*3/4
  taille_mondeDecalageY=taille_mondeY/4
  if(taille_monde>=950)
  {
    
    
    turtle_setangle(0)                                                #La tortue doit regarder vers le haut pour faire le rectangle de haut en bas
    taille_mondeY=taille_mondeY-taille_mondeDecalageY                 #La tortue fait 4 rectangle. On supprime le rectangle pr�c�dent � sa position.
    turtle_setpos(0,taille_mondeY)                                    #Pour un tailleY de 1000 on fera donc 4rec de 250. En pos 750 500 250 0
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#57CEFF", 7)     #Degrade de couleur
    
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#4F93E8", 7)
   
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY   
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#6481FF", 7)
    
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#5C4FE8", 7)
    
  }
  else
  {
    turtle_setangle(0)                                                #La tortue doit regarder vers le haut pour faire le rectangle de haut en bas
    taille_mondeY=taille_mondeY-taille_mondeDecalageY                 #La tortue fait 4 rectangle. On supprime le rectangle pr�c�dent � sa position.
    turtle_setpos(0,taille_mondeY)                                    #Pour un tailleY de 1000 on fera donc 4rec de 250. En pos 750 500 250 0
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#4D423F", 7)     #Degrade de couleur
    
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#332D2A", 7)
    
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY   
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#121110", 7)
    
    turtle_setangle(0)
    taille_mondeY=taille_mondeY-taille_mondeDecalageY
    turtle_setpos(0,taille_mondeY)
    Quadri(taille_monde,taille_mondeDecalageY, 0.5, "#000000", 7)
  }
  
 # Quadri(taille_monde, (taille_monde*3/4), 0.5, "black", 7)
}

Chunk <- function(vecteur,taille_vecteur)
{
  posX <- 0                   # mise a zero des coordonnees ( exemple seed : 4 8 6 7) 
  compteurX <- 1              # 4 definit la longueur du chunk n°1   |  6 definit la longueur du chunk n°2
  compteurY <- 0              # 8 definit la position Y du chunk n°1 |  7 definit la position Y du chunk n°2
  
  for (j in 1:(taille_vecteur/2))     # On divise par 2 car, par exemple pour un seed de 40 chiffres, 
  {                                   # on a bien 20 chunk avec 20 position de Y
    compteurY <- compteurY+2
    
    OutilsChunk(posX*10, vecteur[compteurY]*10, vecteur[compteurX]*10,vecteur[compteurY]*10)
    
    posX <- posX + vecteur[compteurX]
    
    compteurX <- compteurX+2
  }
}

Arbre <- function(vecteur,taille_monde,taille_vecteur)
{
  posX <- 0                   # mise a zero des coordonnees ( exemple seed : 4 8 6 7) 
  compteurX <- 1              # 4 definit la longueur du chunk n°1   |  6 definit la longueur du chunk n°2
  compteurY <- 0              # 8 definit la position Y du chunk n°1 |  7 definit la position Y du chunk n°2
  presence_arbre_position_precedente<-0
  for(i in 1:(taille_vecteur/2 -1))    # un arbre ne peut pas se situer sur le dernier chunk (pour la verification de compteurX + 2)
  {
    compteurY <- compteurY+2
    if (vecteur[compteurY] > 3)    # On evite de faire un seul if a cause des bugs de TRUE / FALSE needed
    {
      if (vecteur[compteurY+2] > 2 && presence_arbre_position_precedente==0)
      {
        OutilsArbre( (posX*10 + vecteur[compteurX]*10 *(3/5)) , vecteur[compteurY]*10, taille_monde,vecteur,taille_vecteur)
        presence_arbre_position_precedente<-1
      }
      else{presence_arbre_position_precedente<-0}
      
    }
    
    posX <- posX + vecteur[compteurX]
    compteurX <- compteurX+2
  }
}

Buisson <- function(vecteur,taille_monde,taille_vecteur)
{
  posX <- 0                   # mise e zero des coordonnees ( exemple seed : 4 8 6 7) 
  compteurX <- 1              # 4 definit la longueur du chunk n°1   |  6 definit la longueur du chunk n°2
  compteurY <- 0              # 8 definit la position Y du chunk n°1 |  7 definit la position Y du chunk n°2
  
  for (k in 1:(taille_vecteur/2-1))
  {
    compteurY <- compteurY+2
    
    if (vecteur[compteurX] > 4)   # On evite de faire un seul if à cause des bugs de TRUE / FALSE needed
    {
      if (vecteur[compteurX+2] > 3)
      {
        OutilsBuisson(posX*10 + vecteur[compteurX]*10 / 2, vecteur[compteurY]*10,vecteur,taille_vecteur)
      }
    }
    
    posX <- posX + vecteur[compteurX]
    
    compteurX <- compteurX+2
  }
}
  
Astre<-function(taille_monde,vecteur,taille)
{

  if (vecteur[taille]>vecteur[1])
  {
    OutilsAstre(taille_monde,taille_monde*3/4,taille_monde)       ## Astre a droite
  }
  else if(vecteur[taille]<vecteur[1])
  {
    OutilsAstre(0,taille_monde*3/4,taille_monde)
  }
  else
  {
    OutilsAstre(taille_monde/2,taille_monde*3/4,taille_monde)
  }
}




  
  #Chunk(posX*10, as.integer(SeedVector[compteurY])*10, as.integer(SeedVector[compteurX])*10, as.integer(SeedVector[compteurY])*10)
  #buisson(posX*10 + (as.integer(SeedVector[compteurX])*10 / 2), as.integer(SeedVector[compteurY])*10,SeedVector)
  #arbre(posX*10 + (as.integer(SeedVector[compteurX])*10 *(3/5)), as.integer(SeedVector[compteurY])*10,somme*10,SeedVector)
  
