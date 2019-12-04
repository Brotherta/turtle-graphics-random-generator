library("grid")
library("TurtleGraphics")
source("Outils.R")


DoIt_Dessin <- function(vecteur,taille_vecteur)
{
  print(vecteur) # Vérification de l'intégrité du seed :D
  taille_monde <- (SommeVecteur(vecteur,taille_vecteur) * 10)
  print(taille_monde)
  Intitialisation(taille_monde)
  Ciel(taille_monde)
  #Astre(taille_monde)
  Chunk(vecteur,taille_vecteur)
  # Arbre(vecteur,taille_monde,taille_vecteur)
  # Buisson(vecteur,taille_monde,taille_vecteur)
  
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
  posX <- 0
  compteurX <- 1
  compteurY <- 0
  
  for (j in 1:(taille_vecteur/2))
  {
    print(j)
    compteurY <- compteurY+2
    
    OutilsChunk(posX*10, vecteur[compteurY]*10, vecteur[compteurX]*10,vecteur[compteurY]*10)
    
    posX <- posX + vecteur[compteurX]
    
    compteurX <- compteurX+2
  }
}

# Arbre <- function()

  
  
  #Chunk(posX*10, as.integer(SeedVector[compteurY])*10, as.integer(SeedVector[compteurX])*10, as.integer(SeedVector[compteurY])*10)
  #buisson(posX*10 + (as.integer(SeedVector[compteurX])*10 / 2), as.integer(SeedVector[compteurY])*10,SeedVector)
  #arbre(posX*10 + (as.integer(SeedVector[compteurX])*10 *(3/5)), as.integer(SeedVector[compteurY])*10,somme*10,SeedVector)
  