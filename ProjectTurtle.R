library("TurtleGraphics")
turtle_init()

##### fonction principale #####


Dessin <- function()
{
  ################## INITI ######################
  
  Seed <- seed_keyboard()
  SeedVector <- c(unlist(strsplit(Seed,"")))
  SeedVector <- better_seed(SeedVector)
  for (compteurVerif in 1:(length(SeedVector)))
  {
    if (SeedVector[compteurVerif] == 0) {SeedVector[compteurVerif] <- 1}
  }
  
  somme <- SommeLongueur(SeedVector)
  turtle_init((somme*10),(somme*10)*3/4,c("clip"))
  turtle_hide()
  print(somme*10)
  print(Seed)
  
  #############CIEL###################
  
  turtle_setpos(0,0)
  Quadri((somme*10),(somme*10)*3/4,0.5,"black",7)
  
  
  ################### Chunk  ##############
  posX <- 0
  compteurX <- 1
  compteurY <- 0
  for(i in 1:(nchar(Seed)/2 -1))
  {
    compteurY <- compteurY+2
    DifferenceHauteurFutur <- as.integer(SeedVector[compteurY]) - as.integer(SeedVector[compteurY+2])
    print(DifferenceHauteurFutur)
    
    
    if(DifferenceHauteurFutur < (-2))
    {
      SeedVector[compteurY+2] <- as.integer(SeedVector[compteurY+2]) + DifferenceHauteurFutur+2
    }
    else if(DifferenceHauteurFutur>2)
    {
      SeedVector[compteurY+2] <- as.integer(SeedVector[compteurY+2]) + DifferenceHauteurFutur-2
    }
    else
    {
      DifferenceHauteurReel <- 0
    }
    
    if (SeedVector[compteurX] > 8)
    {
      if (SeedVector[compteurX+2] > 4)
      {
        arbre(posX*10 + (as.integer(SeedVector[compteurX])*10 *(3/5)), as.integer(SeedVector[compteurY])*10,somme*10,SeedVector)
      }
    }
    
    posX <- posX + as.integer(SeedVector[compteurX])
    
    compteurX <- compteurX+2
  }
  
  posX <- 0
  compteurX <- 1
  compteurY <- 0
  
  for (k in 1:(nchar(Seed)/2-1))
  {
    compteurY <- compteurY+2
    
    if (SeedVector[compteurX] > 4)
    {
      if (SeedVector[compteurX+2] > 3)
      {
        buisson(posX*10 + (as.integer(SeedVector[compteurX])*10 / 2), as.integer(SeedVector[compteurY])*10,SeedVector)
      }
    }
    
    posX <- posX + as.integer(SeedVector[compteurX])
    
    compteurX <- compteurX+2
  }
  
  
  posX <- 0
  compteurX <- 1
  compteurY <- 0
  
  for (j in 1:(nchar(Seed)/2))
  {
    compteurY <- compteurY+2
    
    Chunk(posX*10, as.integer(SeedVector[compteurY])*10, as.integer(SeedVector[compteurX])*10, as.integer(SeedVector[compteurY])*10)
    
    posX <- posX + as.integer(SeedVector[compteurX])
    
    compteurX <- compteurX+2
  }
  
  
}

# arbres_cactus(400,250,5,80,0)


##### Fonctions utiles (pour le moment) ######

buisson <- function(posX,posY,SeedVec)
{
  taille_oursin <- SeedVec
  
  setpos(posX, posY+120)
  Oursin_buisson(taille_oursin)
  setpos(posX, posY+90)
  Oursin_buisson(taille_oursin)
}

Oursin_buisson<-function(c)
{
  for(i in 1:40) #40 premier caractère du seed
  {
    
    couleur <- "#2F5508" #Vert foncé
    
    taille <- as.integer(c[i])*6
    turtle_param(lwd=7)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
    
    couleur <- "#54970D" #Vert clair
    
    taille <- as.integer(c[i])*3
    turtle_param(lwd=7)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
  }
}

arbre <- function(posX,posY,taille,SeedVec)
{
  taille_arbre <- SeedVec
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille),((2/11)*taille),0.5,"brown",7)
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille),((2/11)*taille),0.5,"black",4)
  
  setpos(posX,posY+85)
  Quadri(((2/87)*taille),((2/11)*taille),0.5,"brown",2)
  
  setpos(posX+2,posY+85)
  Quadri(((2/87)*taille),((2/11)*taille),0.5,"brown",0.8)
  
  setpos(posX,posY+85+((2/11)*taille))
  feuillage_arbre(SeedVec,taille)
}

feuillage_arbre<-function(c,taille_arbre)
{
  turtle_setangle(0)
  turtle_forward(taille_arbre*(2/20)/2)
  for(i in 1:40)
  {
    couleur <- "#1E3605" #Vert foncé
    
    taille <- as.integer(c[i])*6
    turtle_param(lwd=6)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
    
    couleur <- "#2E4219" #Vert clair
    
    taille <- as.integer(c[i])*3
    turtle_param(lwd=6)
    turtle_col(couleur)
    turtle_forward(taille)
    turtle_backward(taille)
    turtle_right(360/40)
  }
  turtle_backward(taille_arbre*(2/20)/2)
  
  turtle_right(60)
  
  for(j in 1:6)
  {
    for(k in 1:40)
    {
      couleur <- "#1E3605" #Vert foncé
      
      taille <- as.integer(c[k])*6
      turtle_param(lwd=6)
      turtle_col(couleur)
      turtle_forward(taille)
      turtle_backward(taille)
      turtle_right(360/40)
      
      couleur <- "#2E4219" #Vert clair
      
      taille <- as.integer(c[k])*3
      turtle_param(lwd=6)
      turtle_col(couleur)
      turtle_forward(taille)
      turtle_backward(taille)
      turtle_right(360/40)
    }
    turtle_up()
    turtle_forward(taille_arbre*(2/35))
    turtle_down()
    turtle_left(60)
  }
}


Chunk<-function(posX,posY,Longueur,Hauteur)
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



seed_keyboard<-function()
{
  Seed=""
  scan<-readline(prompt="Veuillez entrez la seed de votre futur dessin : \n")
  #print(scan)
  for(i in 1:nchar(scan))
  {
    #print(i)
    a <- substr(scan,i,i)
    # print(a)
    Seed <- paste(Seed,utf8ToInt(a),sep="")
  }
  #print(Seed)
  while(nchar(Seed) < 40)
  {
    tmp1 <- as.integer(substr(Seed,nchar(Seed),nchar(Seed)))*6497
    Seed <- paste(Seed,as.character(tmp1),sep="")
  }
  if(nchar(Seed) > 40)
  {
    Seed <- substr(Seed,1,nchar(Seed) - nchar(Seed)%%40)
  }
  return(Seed)
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


seed_keyboardFAKE<-function()
{
  return ("212922282821")
}



SommeLongueur <- function(c)
{
  i<-1
  somme <- 0
  while (i < 40)
  {
    somme <- somme + as.integer(c[i])
    i <- i+2
  }
  return(somme)
}





setpos<-function(x,y)
{
  turtle_up()
  turtle_setpos(x,y)
  turtle_down()
  turtle_setangle(0)
}




##### fonctions inutilisées pour le moment #####



EspaceY<-function(Distance)
{
  
  x<-turtle_getpos()["x"]
  y<-turtle_getpos()["y"]
  turtle_up()
  turtle_goto(as.integer(substr(x,1,10)),as.integer(substr(y,1,10))-Distance)
  turtle_down()
}
EspaceX<-function(Distance)
{
  
  x<-turtle_getpos()["x"]
  y<-turtle_getpos()["y"]
  turtle_up()
  turtle_goto(as.integer(substr(x,1,10))-Distance,as.integer(substr(y,1,10)))
  turtle_down()
}

Turtle_Coloriage<-function(color)
{
  turtle_goto(0)
  turtle_setangle(0)
  QuadraFill(20,100)
}
turtle_start<-function()
{
  turtle_init(100)
  turtle_hide()
}

Fill<-function(Longueur,Largeur=Longueur)
{
  if(is.null(Largeur)){Largeur<-Longueur} # Un carré wesh
  turtle_param("yellow",lwd=Largeur)
  turtle_forward(Largeur)
  turtle_setangle(90)
  turtle_forward(Longueur)
}


better_seed <- function(c)
{
  compteur <- 1
  for (i in 1:(length(c)/2 -1))
  {
    if(c[compteur] < 5)
    {
      if(c[compteur+2] > 5)
      {
        c[compteur] <- 7
      }
      # else if (c[compteur+2] < 5)
      # {
      #   c[compteur] <- 9
      # }
    }
    compteur <- compteur +2
  }
  return(c)
}










