
library(raster)
library(RNetCDF)

#schalrjahre 366 Tage

files.Tmax3017 <- list.files() #After it set the working dictenory to the folder im am working in now
nv<- length(files.Tmax3017) #changed that to VectorBrickl from BrickVect, should work
n <- length(shapeUs.RECB$STATEFP)
nv
ns <- 366
nj <- 365
funmaxdiff30 <- function(x){ifelse(x>30,x-30,0)}

#each layer is a day

#this is to calculate the sum of dummies for negative days
#schalrjahre 366 Tage
DF.Tmax30.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3017[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff30)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) #WORKS _ seem to be a problem
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff30) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.17[row,1] <- USSHPCB$STATEFP[x] #Bringing the STate FTP code in  # here the ssignment of the number wokred
      DF.Tmax30.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time #date doensr works here
      DF.Tmax30.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

