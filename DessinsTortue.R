library("grid")
library("TurtleGraphics")

## VALENIN MASCARO
## VIDAL-MAZUY ANTOINE

## POUR LANCER LA FONCTION : APPELER DoIt() SANS PARAMETRE
## UN FICHIER PDF DesinTortue.pdf SERA CREER DANS LE REPERTOIRE COURANT

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
  #la taille est, selon la doc, defini en pouces, on met 17 pour comble la plupart des ecran :p
  
}


###################################################################################################################################
###################################################################################################################################
##########################################   PARTIE DE CREATION DU SEED     #######################################################
###################################################################################################################################
###################################################################################################################################


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
  tmp<-utf8ToInt(scan)*1234 # tmp prend la valeur de scan, en int, et agrandi par un nombre defini au hasard
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


###################################################################################################################################
###################################################################################################################################
##########################################   PARTIE DE DESSIN DE LA TORTUE    #####################################################
###################################################################################################################################
###################################################################################################################################


DoIt_Dessin <- function(vecteur,taille_vecteur)  # Fonction principale qui dessine l'integralite du dessin final
{
  biome <- Biome(vecteur)
  print(vecteur) # Verification de l'integrite du seed :D
  taille_monde <- (SommeVecteur(vecteur,taille_vecteur) * 10)
  print(taille_monde) # debug
  Initialisation(taille_monde)
  Ciel(taille_monde)
  Astre(taille_monde,vecteur,taille_vecteur)
  Chunk(vecteur,taille_vecteur,biome)
  Arbre(vecteur,taille_monde,taille_vecteur,biome)
  Buisson(vecteur,taille_monde,taille_vecteur,biome)
}

Biome <- function(vecteur)
{
  if (vecteur[5] <= 3)
  {
    biome = 1    ### biome neige
  }
  else if (vecteur[5] <= 6)
  {
    biome = 2    ### biome for�t claire
  }
  else
  {
    biome = 3    ### biome for�t corruption
  }
  return (biome)
} 

Initialisation <- function(taille_monde)
{
  turtle_init(taille_monde, (taille_monde*3/4), c("clip"))
  turtle_hide()
}

Ciel <- function(taille_monde)
{
  taille_mondeY=taille_monde*3/4
  taille_mondeDecalageY=taille_mondeY/4
  if(taille_monde>=1010)
  {
    
    
    turtle_setangle(0)                                                #La tortue doit regarder vers le haut pour faire le rectangle de haut en bas
    taille_mondeY=taille_mondeY-taille_mondeDecalageY                 #La tortue fait 4 rectangles. On supprime le rectangle precedent � sa position.
    turtle_setpos(0,taille_mondeY)                                    #Pour une taille Y de 1000 on fera donc 4rec de 250. En pos 750 500 250 0
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
    taille_mondeY=taille_mondeY-taille_mondeDecalageY                 #La tortue fait 4 rectangles. On supprime le rectangle precedent � sa position.
    turtle_setpos(0,taille_mondeY)                                    #Pour une taille Y de 1000 on fera donc 4rec de 250. En pos 750 500 250 0
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

Chunk <- function(vecteur,taille_vecteur,biome)
{
  posX <- 0                   # mise a zero des coordonnees ( exemple seed : 4 8 6 7) 
  compteurX <- 1              # 4 definit la longueur du chunk n°1   |  6 definit la longueur du chunk n°2
  compteurY <- 0              # 8 definit la position Y du chunk n°1 |  7 definit la position Y du chunk n°2
  
  for (j in 1:(taille_vecteur/2))     # On divise par 2 car, par exemple pour un seed de 40 chiffres, 
  {                                   # on a bien 20 chunk avec 20 position de Y
    compteurY <- compteurY+2
    
    OutilsChunk(posX*10, vecteur[compteurY]*10, vecteur[compteurX]*10,vecteur[compteurY]*10,biome)
    
    posX <- posX + vecteur[compteurX]
    
    compteurX <- compteurX+2
  }
}

Arbre <- function(vecteur,taille_monde,taille_vecteur,biome)
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
        OutilsArbre( (posX*10 + vecteur[compteurX]*10 *(3/5)) , vecteur[compteurY]*10, taille_monde,vecteur,taille_vecteur,biome)
        presence_arbre_position_precedente<-1
      }
      else{presence_arbre_position_precedente<-0}
      
    }
    
    posX <- posX + vecteur[compteurX]
    compteurX <- compteurX+2
  }
}

Buisson <- function(vecteur,taille_monde,taille_vecteur,biome)
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
        OutilsBuisson(posX*10 + vecteur[compteurX]*10 / 2, vecteur[compteurY]*10,vecteur,taille_vecteur,biome)
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


###################################################################################################################################
###################################################################################################################################
#############################################   PARTIE OUTILS DE LA TORTUE    #####################################################
###################################################################################################################################
###################################################################################################################################


#############################################################################
################### DESSIN DU CHUNK #########################################
#############################################################################


OutilsChunk<-function(posX,posY,Longueur,Hauteur,biome)
{
  ## on definit les couleurs des biomes 
  
  if (biome == 1) couleur = "#FDFEFE" ## blanc
  else if (biome == 2) couleur = "#2C833D" ## vert clair
  else if (biome == 3) couleur = "#453766" ## violet corruption
  
  
  # On dessine successivement les diferentes couches de chunks,
  # pour donner du relief.
  
  setpos(posX,posY+40+30)
  Quadri(Longueur,10,0.5,couleur,7)
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

OutilsArbre <- function(posX,posY,taille_monde,vecteur,taille_vecteur,biome)
{
  ## on definit les couleurs des biomes 
  
  if (biome == 1) couleur <- "#FDFEFE" ## blanc
  else if (biome == 2) couleur <- "#1E3605" ## vert clair
  else if (biome == 3) couleur <- "#3F2A72" ## violet corruption
  
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
  
  OutilsFeuillage(vecteur,taille_monde,taille_vecteur,couleur,biome)
}

OutilsFeuillage<-function(vecteur,taille_monde,taille_vecteur,couleur_biome,biome)
{
  if (biome == 2) couleur2 <- "#2E4219" ## vert clair pour le biome corruption
  else couleur2 <- "#1E3605" #vert fonce
  
  turtle_setangle(0)
  turtle_forward(taille_monde*(2/20)/2) # on deplace la tortue au dessus du tronc,
  # et on fait un premier feuillage central.
  for(i in 1:taille_vecteur)
  {
    # On commence par un Oursin de la couleur du biome, puis plus petit un Oursin vert fonc�.
    couleur <- couleur_biome
    
    taille <- vecteur[i]*6
    turtle_param(lwd=6)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
    
    couleur <- couleur2
    
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
      # On commence par un Oursin de la couleur du biome, puis plus petit un Oursin vert fonce.
      
      couleur <- couleur_biome
      
      taille <- vecteur[k]*6
      turtle_param(lwd=6)
      turtle_col(couleur)
      turtle_forward(taille)
      turtle_backward(taille)
      turtle_right(360/taille_vecteur)
      
      couleur <- couleur2
      
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

OutilsBuisson <- function(posX,posY,vecteur,taille_vecteur,biome)
{
  ## on definit les couleurs des biomes 
  
  if (biome == 1) couleur = "#FDFEFE" ## blanc
  else if (biome == 2) couleur = "#2F5508" ## vert fonce
  else if (biome == 3) couleur = "#2E1E53"  ## violet corruption
  
  # Le buisson se compose de la même maniere que les feuillages des arbres.
  # On commence part un oursin Vert fonce et un plus petit vert clair.
  # On repete cette action deux fois.
  
  setpos(posX, posY+120)
  OursinBuisson(vecteur,taille_vecteur,couleur,biome)
  
  setpos(posX, posY+90)
  OursinBuisson(vecteur,taille_vecteur,couleur,biome)
}

OursinBuisson<-function(vecteur,taille_vecteur,couleur_biome,biome)
{
  if (biome == 3) couleur2 <- "#1E3605" ## vert fonce pour le biome corruption
  else couleur2 <- "#54970D" #vert clair
  
  for(i in 1:taille_vecteur)
  {
    couleur <- couleur_biome
    
    taille <- vecteur[i]*6
    turtle_param(lwd=7)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/taille_vecteur)
    
    couleur <- couleur2
    
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
###################  Outils de l'Astre   ####################################
#############################################################################

OutilsAstre<-function(posX,posY,taille_monde)
{
  setpos(posX,posY)
  j<-0
  if(taille_monde>=1010)
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
  
  for(i in 1:360)    ## on fait un rond complet, meme si on n'en verra qu'une partie. On se garde simplement la possibilite de faire apparaitre un astre complet
  {
    turtle_setangle(i)
    turtle_forward(taille_monde/10)
    turtle_backward(taille_monde/10)
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

setpos<-function(x,y) # Cette fonction est utile,
{                     # pour deplacer la tortue sans faire de trait.
  turtle_up()
  turtle_setpos(x,y)
  turtle_down()
  turtle_setangle(0)
}

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

#############################################################################
#############################################################################