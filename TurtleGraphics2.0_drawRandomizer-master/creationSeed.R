DoIt_Seed<-function(taille)
{
  scan<-EntreeClavier()
  vecteur<-numeric(taille) # creation du vecteur qui contiendra la suite de chiffre permettant de generer le dessin
  vecteur <- creationDuSeed(scan,vecteur,taille)
  return(vecteur)
}




EntreeClavier<-function()
{
  scan<-readline(prompt="Veuillez entrez votre seed :D (max 30 caracteres):  ")    # La suite de caractere qui definira le dessin
  if (nchar(scan) > 30) 
  {
    print("Seed trop longue ! (max 30 caracteres) :  ")
    while(nchar(scan) > 30)
    {
      scan<-readline(prompt="Seed  (max 30 caracteres) :  ")
    }
  }
  return (scan)
}

creationDuSeed<-function(scan,vecteur,taille)
{
  vecteurTemporaire<-CreationDuvecteurTemporaire(scan,taille)
  i<-1
  y<-2
  while(i<=taille)           
  {
    if(vecteurTemporaire[y]==0 || vecteurTemporaire[y]==1){vecteurTemporaire[y]<-3}
    if((vecteurTemporaire[i]==0 || vecteurTemporaire[i]==1)){vecteurTemporaire[i]<-3}
    if(y+2<=taille)
    {
      DifferenceY<-vecteurTemporaire[y+2]-vecteurTemporaire[y]
      if(DifferenceY>2)
      {
        vecteurTemporaire[y+2]<-vecteurTemporaire[y+2]-DifferenceY+2
      }
      else if(DifferenceY<(-2))
      {
        vecteurTemporaire[y+2]<-vecteurTemporaire[y]-2
      }
    }  
    vecteur[i]<-vecteurTemporaire[i]
    vecteur[y]<-vecteurTemporaire[y]
    i<-i+2
    y<-y+2
  }                                                    
  return(vecteur)
}

CreationDuvecteurTemporaire<-function(scan,taille)
{
  tmp<-utf8ToInt(scan)*1234 # tmp prend la valeur de scan, en int, et agrandi par un nombre défini au hasard
  if(nchar(tmp)<taille)     # Si la taille de tmp est trop faible pour remplir le vecteur voulu, on l'agrandi (dans le while)
  {
    hasard<-5678
    while(nchar(tmp)<taille)
    {
      tmp<-paste(tmp,as.character(utf8ToInt(scan)*hasard),sep="",collapse=NULL)
      hasard<-hasard+33
      
    }
  }
  #print(tmp)
  tmpNum<- hashDuSeed(tmp,taille)
  return (tmpNum)
}

hashDuSeed <- function(tmp,taille)
{
  tmp_entier <- paste(tmp, collapse="")
  tmp_vecteur <- numeric(nchar(tmp_entier))
  j<-0
  #print(tmp_entier)
  for (l in 1:(nchar(tmp_entier)))
  {
    tmp_vecteur[l] <- as.integer(substr(tmp_entier,l,l))
    
  }
  
  tmpNum<-numeric(taille) # pour eviter des problemes de type, on passe tmp en numeric et on lui affecte des integer
  #print(tmp_vecteur)
  for(acc in 1:taille)
  {
    j<-j+length(tmp_vecteur) / taille
    tmpNum[acc]<-tmp_vecteur[j]
    
  }
  return (tmpNum)
}



