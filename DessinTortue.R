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
  #Astre(vecteur,taille_monde)
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
  turtle_setpos(0,0)
  Quadri(taille_monde, (taille_monde*3/4), 0.5, "black", 7)
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
    if (vecteur[compteurY] > 6)    # On evite de faire un seul if a cause des bugs de TRUE / FALSE needed
    {
      if (vecteur[compteurY+2] > 4 && presence_arbre_position_precedente==0)
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

Astre<-function()
{
  turtle_init()
  turtle_hide()
  x<-6
  for(i in 1:60)
  {
    turtle_setangle(-20+x)
    turtle_forward(2)
    x<-x+6
  }
}

Astre2<-function(vecteur,taille_vecteur=40,taille_monde=800)
{
  taille<-taille_monde/8  ## ni trop gros ni trop petit. Au pifff
  if
  #setpos(taille_monde,(((taille_monde)*3/4)))  
  for(increment in 360:0)
  {
    turtle_setangle(increment)
    turtle_param(lwd=7)
    turtle_forward(taille)
    turtle_backward(taille)
    
  }
}


  
  
  #Chunk(posX*10, as.integer(SeedVector[compteurY])*10, as.integer(SeedVector[compteurX])*10, as.integer(SeedVector[compteurY])*10)
  #buisson(posX*10 + (as.integer(SeedVector[compteurX])*10 / 2), as.integer(SeedVector[compteurY])*10,SeedVector)
  #arbre(posX*10 + (as.integer(SeedVector[compteurX])*10 *(3/5)), as.integer(SeedVector[compteurY])*10,somme*10,SeedVector)
  
