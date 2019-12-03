source("creationSeed.R")
source("DessinTortue.R")
#source(turtleDessin.r)



DoIt<-function(taille = 40)
{
  if(taille%%2!=0)
  {
    taille<-taille+1
  }
  vecteur<-numeric(taille) # création du vecteur qui contiendra la suite de chiffre permettant de généré le dessin
  vecteur<-DoIt_Seed(vecteur,taille) #  On set les valeurs du vecteur anciennement vide
  Dessin(vecteur,taille)
}



