library("TurtleGraphics")


seed_keyboard<-function()
{
  Seed=""
  scan<-readline(prompt="Veuillez entrez la seed de votre futur dessin : \n")
  for(i in 1:nchar(scan))
  {
    a <- substr(scan,i,i)
    Seed <- paste(Seed,utf8ToInt(a),sep="")
  }
  while(nchar(Seed) < 40)
  {
    tmp1 <- as.integer(substr(Seed,nchar(Seed),nchar(Seed)))*6497
    Seed <- paste(Seed,as.character(tmp1),sep="")
  }
  if(nchar(Seed) > 40)
  {
    Seed <- substr(Seed,1,nchar(Seed) - nchar(Seed)%%40)
  }
  print(Seed)
}

Quadri <- function(l,L,i=1,colour) #l = Longueur L = largeur
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
  return ("572657264544")
}

Dessin<-function()
{
  ################## INITI ######################
  Seed<-seed_keyboard()
  SeedVector<-c(unlist(strsplit(Seed,"")))
 # somme<-as.integer(SeedVector[1])+as.integer(SeedVector[3])+as.integer(SeedVector[5])+as.integer(SeedVector[7])+as.integer(SeedVector[9])+as.integer(SeedVector[11])
  somme <- SommeLongueur(SeedVector)
  turtle_init((somme*10),(somme*10)*3/4,c("clip"))
  turtle_hide()
  ###############################################
  
  #############CIEL###################
  turtle_setpos(0,0)
  Quadri((somme*10),(somme*10)*3/4,1,"black")
  #####################################
  
  ################### Chunk  ##############
  posX<-0
  compteurX<-1
  compteurY<-0
  
  for(i in 1:somme)
  {
    compteurY<-compteurY+2
    
    Chunk(posX*10,as.integer(SeedVector[compteurY])*10,as.integer(SeedVector[compteurX])*10,as.integer(SeedVector[compteurY])*10)
    
    posX<-posX+as.integer(SeedVector[compteurX])
    
    compteurX<-compteurX+2
  }
   
}
 

Chunk<-function(posX,posY,Longueur,Hauteur)
{
  setpos(posX,posY)
  Quadri(Longueur,10,1,"green")
  setpos(posX,posY-(Hauteur+1))
  Quadri(Longueur,Hauteur,1,"brown")
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


testwhile <- function()
{
  i <- 1
  while (i < 40)
  {
    print(i)
    i <- i+2
  }
  print(i)
}

