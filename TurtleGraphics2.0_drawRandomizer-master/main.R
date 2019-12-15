source("creationSeed.R")
source("DessinTortue.R")
#source(turtleDessin.r)



DoIt<-function(taille = 40)
{
  if(taille%%2!=0)
  {
    taille<-taille+1
  }
  vecteur<-DoIt_Seed(taille) #  On set les valeurs du vecteur anciennement vide
  #print(vecteur)
  DoIt_Dessin(vecteur,taille)
  
  dev.print(pdf,'DessinTortue.pdf',paper="USr",width=17,) # Une fois le dessin fini on enregistre au format pdf 
  #la taille est, selon la doc, défini en pouces, on met 17 pour comblé la plupart des écran :p
  
}
