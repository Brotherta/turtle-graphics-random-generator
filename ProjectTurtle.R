library("TurtleGraphics")
turtle_init()


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
Fill<-function(Longueur,Largeur=Longueur)
{
  if(is.null(Largeur)){Largeur<-Longueur} # Un carré wesh
  turtle_param("yellow",lwd=Largeur)
  turtle_forward(Largeur)
  turtle_setangle(90)
  turtle_forward(Longueur)
  
}



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

Quadri <- function(l,L,i,colour) #l = Longueur L = largeur
{
  k <- l
  l <- L
  turtle_param(colour,lwd = 7)
  
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

Dessin<-function()
{
  ################## INITI ######################
  Seed<-seed_keyboard()
  SeedVector<-c(unlist(strsplit(Seed,"")))
  for(compteurVerif in 1:(length(SeedVector)))
  {
    if (SeedVector[compteurVerif]==0){SeedVector[compteurVerif]<-1}
  }
  somme<-SommeLongueur(SeedVector)
  turtle_init((somme*10),(somme*10)*3/4,c("clip"))
  turtle_hide()
  ###############################################
  
  #############CIEL###################
  turtle_setpos(0,0)
  Quadri((somme*10),(somme*10)*3/4,0.5,"black")
  #####################################
  
  ################### Chunk  ##############
  posX<-0
  compteurX<-1
  compteurY<-0
  
  for(i in 1:(nchar(Seed)/2))
  {
    compteurY<-compteurY+2
    
    Chunk(posX*10,as.integer(SeedVector[compteurY])*10,as.integer(SeedVector[compteurX])*10,as.integer(SeedVector[compteurY])*10)
    DifferenceHauteurFutur<-as.integer(SeedVector[compteurY])-as.integer(SeedVector[compteurY+2])
    print(DifferenceHauteurFutur)
    if(DifferenceHauteurFutur<(-2))
    {
      SeedVector[compteurY+2]<-as.integer(SeedVector[compteurY+2])+DifferenceHauteurFutur+2
    }
    else if(DifferenceHauteurFutur>2)
    {
      SeedVector[compteurY+2]<-as.integer(SeedVector[compteurY+2])+DifferenceHauteurFutur-2
    }
    else
    {
      DifferenceHauteurReel<-0
    }
    posX<-posX+as.integer(SeedVector[compteurX])
    
    compteurX<-compteurX+2
  }
  
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


Chunk<-function(posX,posY,Longueur,Hauteur)
{
  setpos(posX,posY+40)
  Quadri(Longueur,10,0.5,"green")
  setpos(posX,0)
  Quadri(Longueur,Hauteur+40,0.5,"brown")
}


setpos<-function(x,y)
{
  turtle_up()
  turtle_setpos(x,y)
  turtle_down()
  turtle_setangle(0)
}
