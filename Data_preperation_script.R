#Script for the processing of the weather data

library(raster)
library(RNetCDF)
library(rnoaa)
library(USAboundaries)
library(plyr)
library(rgeos)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(gtrendsR)
library(fixest)
#Obtaining the shapefile of the contagious US-States
shapeUS <- readOGR(dsn = "Gridd_Data", layer = "tl_2017_us_state")

#Reducing the file to the Corn Belt files
USSHPCB <- subset(shapeUS, STATEFP == "17"| STATEFP == "18" |STATEFP == "19"|STATEFP == "20"|STATEFP == "21"|STATEFP == "26"|STATEFP == "27"|STATEFP == "29"|STATEFP == "31"|STATEFP == "38"|STATEFP == "39"|STATEFP == "46"|STATEFP == "55" )

n <- length(shapeUs.RECB$STATEFP)
#lengths for leap years
ns <- 366
#length for normal years
nj <- 365

#Temperature threshold function 
funmaxdiff30 <- function(x){ifelse(x>30,x-30,0)} 
funmaxdiff31 <- function(x){ifelse(x>31,x-31,0)}
funmaxdiff32 <- function(x){ifelse(x>32,x-32,0)}
funmaxdiff33 <- function(x){ifelse(x>33,x-33,0)}
funmaxdiff35 <- function(x){ifelse(x>35,x-35,0)}
funmindiff <- function(x){ifelse(x<0,x,0)}


#Year 2016 for Tmax for temperature threshold 30 
files.Tmax3016 <- list.files() #The working dictionary is set manually 
nv<- length(files.Tmax3016)
DF.Tmax30.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns)) #Creating a empty data frame for the loop
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3016[i]) #each brick is a month, hence here we retrieve the monthly organised files 
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff30)  #here the EDDs caclulation is applied
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){ 
    Rasterlayer2 <- subset(Gridbrick2, z) #wee need this file format to assign later the date to the data frame #each layer represents one day.
    Rasterlayer1 <- subset(GridBrick1, z) #in this file we find the EDDs 

    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.16[row,1] <- USSHPCB$STATEFP[x] #assinging the particular State FP 
      DF.Tmax30.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")   #ssining and formating of the date
      DF.Tmax30.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE) # here the shapefile comes hin to assing the cell vlaues to the respective state
      
    }
  }
}


#Year 2016 Tmax for treshold 31
files.Tmax3116 <- list.files() 
nv<- length(files.Tmax3116) 

DF.Tmax31.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3116[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff31)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax31.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax31.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax31.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}


#Year 2016 Tmax for treshold 32
files.Tmax3216 <- list.files() 
nv<- length(files.Tmax3216) 

DF.Tmax32.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3216[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff32)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax32.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax32.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax32.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Year 2016 Tmax for treshold 33
files.Tmax3316 <- list.files() 
nv<- length(files.Tmax3316) 

DF.Tmax33.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3316[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff33)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax33.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax33.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax33.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Year 2016 Tmax for Threshold 35

files.Tmax3516 <- list.files() 
nv<- length(files.Tmax3516) 

DF.Tmax35.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3516[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff35)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax35.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax35.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax35.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}


#Year 2017 Tmax for Threshold 30

files.Tmax3017 <- list.files() 
nv<- length(files.Tmax3017) 

DF.Tmax30.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3017[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff30)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax30.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax30.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

# 31 2017
files.Tmax3117 <- list.files() 
nv<- length(files.Tmax3117) 

DF.Tmax31.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3117[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff31)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff31) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax31.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax31.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax31.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#32 2017

files.Tmax3217 <- list.files() 
nv<- length(files.Tmax3217) 

DF.Tmax32.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3217[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff32)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff32) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax32.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax32.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax32.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2017 33


files.Tmax3317 <- list.files() 
nv<- length(files.Tmax3317) 

DF.Tmax33.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3317[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff33)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff33) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax33.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax33.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax33.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

View(DF.Tmax33.17)

#2017 35
files.Tmax3517 <- list.files() 
nv<- length(files.Tmax3517) 


DF.Tmax35.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3517[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff35)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax35.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax35.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax35.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

# 2018 30
files.Tmax3018 <- list.files() 
nv<- length(files.Tmax3018) 


DF.Tmax30.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3018[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff30)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax30.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax30.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2018 31

#Tmax 16 18 31


files.Tmax3118 <- list.files() 
nv<- length(files.Tmax3118) 


DF.Tmax31.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3118[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff31)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff31) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax31.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax31.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax31.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2018 32
files.Tmax3218 <- list.files() 
nv<- length(files.Tmax3218) 

DF.Tmax32.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3218[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff32)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff32) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax32.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax32.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax32.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2018 33



files.Tmax3318 <- list.files() 
nv<- length(files.Tmax3318) 

DF.Tmax33.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3318[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff33)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff33) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax33.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax33.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax33.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2018 35


files.Tmax3518 <- list.files() 
nv<- length(files.Tmax3518) 


DF.Tmax35.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3518[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff35)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax35.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax35.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax35.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2019 30

files.Tmax3019 <- list.files() 
nv<- length(files.Tmax3019) 

DF.Tmax30.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3019[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff30)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax30.19[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax30.19[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2019 31
files.Tmax3119 <- list.files() 
nv<- length(files.Tmax3119) 


DF.Tmax31.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3119[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff31)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff31) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax31.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax31.19[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax31.19[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2019 32
files.Tmax3219 <- list.files() 
nv<- length(files.Tmax3219) 


DF.Tmax32.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3219[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff32)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff32) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax32.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax32.19[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax32.19[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2019 33
#Tmax 16 18 33

files.Tmax3318 <- list.files() 
nv<- length(files.Tmax3318) 


DF.Tmax33.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3318[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff33)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff33) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax33.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax33.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax33.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}


#2019 35

files.Tmax3519 <- list.files() 
nv<- length(files.Tmax3519) 


DF.Tmax35.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3519[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff35)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax35.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax35.19[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax35.19[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2020 30
files.Tmax3020 <- list.files() 
nv<- length(files.Tmax3020) 


DF.Tmax30.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3020[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax30.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax30.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax30.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#2020 31
files.Tmax3120 <- list.files() 
nv<- length(files.Tmax3120) 


DF.Tmax31.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3120[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff31)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff31) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax31.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax31.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax31.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2020 32
files.Tmax3220 <- list.files() 
nv<- length(files.Tmax3220) 

DF.Tmax32.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3220[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff32)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff32) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax32.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax32.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax32.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2020 33

files.Tmax3320 <- list.files() 
nv<- length(files.Tmax3320) 

DF.Tmax33.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3320[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff33)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    #Rasterlayerc <- calc(Rasterlayer, funmaxdiff33) #here is the problem
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax33.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax33.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax33.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#2020 35
files.Tmax3520 <- list.files() 
nv<- length(files.Tmax3520) 

DF.Tmax35.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmax3520[i])
  
  Gridbrick2 <- calc(GridBrick1, funmaxdiff35)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
   
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmax35.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmax35.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d")
      DF.Tmax35.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Precipitaion Data

#PRecipitation 2016
files.precp16  <- list.files()
nv<- length(files.precp16) 
n <- length(shapeUs.RECB$STATEFP)

DF.Precp.16 <- as.data.frame(matrix(ncol = 3, nrow = n*nv)) 




files.precp16 <- list.files() 


GridBrick <- brick(files.precp16[1])


GridBrick
View(gtrends0105df16)



DF.Precp.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick <- brick(files.precp16[i])
  cat("\nmonth ", i)
  
  for (z in 1: nlayers(GridBrick) ){
    Rasterlayer <- subset(GridBrick, z) 
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Precp.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Precp.16[row,3] <-format(as.Date(names(Rasterlayer), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time
      DF.Precp.16[row,2] <- cellStats(mask(Rasterlayer, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Precipitation 2017

nv<- length(files.precp17) 
n <- length(shapeUs.RECB$STATEFP)

DF.Precp.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nv)) 

files.precp17 <- list.files() 

GridBrick <- brick(files.precp17[1])

DF.Precp.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick <- brick(files.precp17[i])
  cat("\nmonth ", i)
  
  for (z in 1: nlayers(GridBrick) ){
    Rasterlayer <- subset(GridBrick, z) 
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Precp.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Precp.17[row,3] <-format(as.Date(names(Rasterlayer), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time
      DF.Precp.17[row,2] <- cellStats(mask(Rasterlayer, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#Precipitation 2018

files.precp18 <- list.files() 
nv<- length(files.precp18) 
n <- length(shapeUs.RECB$STATEFP)


DF.Precp.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick <- brick(files.precp18[i])
  cat("\nmonth ", i)
  
  for (z in 1: nlayers(GridBrick) ){
    Rasterlayer <- subset(GridBrick, z) 
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Precp.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Precp.18[row,3] <-format(as.Date(names(Rasterlayer), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time
      DF.Precp.18[row,2] <- cellStats(mask(Rasterlayer, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Precp 2019
files.precp19 <- list.files() 
nv<- length(files.precp19) 
n <- length(shapeUs.RECB$STATEFP)


DF.Precp.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick <- brick(files.precp19[i])
  cat("\nmonth ", i)
  
  for (z in 1: nlayers(GridBrick) ){
    Rasterlayer <- subset(GridBrick, z) 
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Precp.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Precp.19[row,3] <-format(as.Date(names(Rasterlayer), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time
      DF.Precp.19[row,2] <- cellStats(mask(Rasterlayer, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}
#Precip 2020
files.precp20 <- list.files() 
nv<- length(files.precp20) 
n <- length(shapeUs.RECB$STATEFP)

DF.Precp.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick <- brick(files.precp20[i])
  cat("\nmonth ", i)
  
  for (z in 1: nlayers(GridBrick) ){
    Rasterlayer <- subset(GridBrick, z) 
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Precp.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Precp.20[row,3] <-format(as.Date(names(Rasterlayer), format = "X%Y.%m.%d"), format = "%Y.%m.%d") #defining the time
      DF.Precp.20[row,2] <- cellStats(mask(Rasterlayer, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}


# Cold extreeme day calculation
#Cold 2016
files.Tmin16 <- list.files() 
nv<- length(files.Tmin16) 
n <- length(shapeUs.RECB$STATEFP)

DF.Tmin.16 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmin16[i])
  
  Gridbrick2 <- calc(GridBrick1, funmindiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmin.16[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmin.16[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") 
      DF.Tmin.16[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Cold 2017
files.Tmin17 <- list.files() 
nv<- length(files.Tmin17) 
n <- length(shapeUs.RECB$STATEFP)
nv


funmindiff <- function(x){ifelse(x<0,x,0)}





DF.Tmin.17 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmin17[i])
  
  Gridbrick2 <- calc(GridBrick1, funmindiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmin.17[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmin.17[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") 
      DF.Tmin.17[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Cold 2018
files.Tmin18 <- list.files() 
nv<- length(files.Tmin18) 
n <- length(shapeUs.RECB$STATEFP)

DF.Tmin.18 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmin18[i])
  
  Gridbrick2 <- calc(GridBrick1, funmindiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmin.18[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmin.18[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") 
      DF.Tmin.18[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Cold 2019
files.Tmin19 <- list.files() 
nv<- length(files.Tmin19) 
n <- length(shapeUs.RECB$STATEFP)


DF.Tmin.19 <- as.data.frame(matrix(ncol = 3, nrow = n*nj))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmin19[i])
  
  Gridbrick2 <- calc(GridBrick1, funmindiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmin.19[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmin.19[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") 
      DF.Tmin.19[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}

#Cold 2020
files.Tmin20 <- list.files() 
nv<- length(files.Tmin20) 
n <- length(shapeUs.RECB$STATEFP)


DF.Tmin.20 <- as.data.frame(matrix(ncol = 3, nrow = n*ns))
row = 0
for (i in 1:nv){
  GridBrick1 <- brick(files.Tmin20[i])
  
  Gridbrick2 <- calc(GridBrick1, funmindiff)
  cat("\nmonth ", i)
  for (z in 1: nlayers(GridBrick1) ){
    Rasterlayer2 <- subset(Gridbrick2, z) 
    Rasterlayer1 <- subset(GridBrick1, z)
    
    cat("\nday ", z, "\n")
    
    for (x in 1:length(USSHPCB$STATEFP)) { 
      cat("state ", x, "; ")
      
      STATEFPLOOP <- USSHPCB$STATEFP[x]
      row = row + 1
      cat("row ", row, "; ")
      DF.Tmin.20[row,1] <- USSHPCB$STATEFP[x] 
      DF.Tmin.20[row,3] <-format(as.Date(names(Rasterlayer1), format = "X%Y.%m.%d"), format = "%Y.%m.%d") 
      DF.Tmin.20[row,2] <- cellStats(mask(Rasterlayer2, subset(USSHPCB, STATEFP == STATEFPLOOP)), stat = "sum", na.rm=TRUE)
      
    }
  }
}


#Retrieving Google Trends Data 

geogtrends <- gtrendsR::countries
geogtrends 
USgeo <- filter(geogtrends, country_code == "US")

USgeo.states <-  slice(USgeo, 1:52)

# Creating subgroups
CBgeo.ISO <- filter(USgeo.states, name ==  "NORTH DAKOTA" | name ==  "SOUTH DAKOTA" |  name ==  "NEBRASKA" | name ==  "KANSAS" | name ==  "SOUTH DAKOTA" | name ==  "OHIO" | name ==  "MINNESOTA" | name ==  "WISCONSIN"| name ==  "IOWA" |name ==  "MISSOURI"|name ==  "ILLINOIS" |name ==  "MICHIGAN" |name ==  "INDIANA"|name ==  "KENTUCKY"|name ==  "OHIO")
CBgeo.ISO
CBgeo.ISO15 <- (slice(CBgeo.ISO, 1:5))
CBgeo.ISO610<- (slice(CBgeo.ISO, 6:10))
CBgeo.ISO1113<-(slice(CBgeo.ISO, 11:13))

lgeo1 <- nrow(CBgeo.ISO15)
lgeo2 <- nrow(CBgeo.ISO610)
lgeo3 <- nrow(CBgeo.ISO1113)

#Data Obtained on the 17.02..2020

gtrends1113df16 <- as.data.frame(matrix(ncol = 3, nrow = 261*3))
for (i in 1:lgeo3) {
  
  
  trendstest <- gtrends("Crop Insurance", geo = CBgeo.ISO1113[i,2], time = "2016-01-01 2020-12-31")
  interest.ot <- trendstest$interest_over_time
  interest.ot2 <- interest.ot[c(1,2,4)]
  interestn <- nrow(interest.ot2)
  for (x in 1: interestn){
    rgt <- x +(261*(i-1))
    gtrends1113df16[rgt,1] <- interest.ot2$geo[x]
    gtrends1113df16[rgt,2] <- interest.ot2$hits[x]
    gtrends1113df16[rgt,3] <- as.character(interest.ot2$date[x])  }}


gtrends1113df16
View(gtrends1113df16)
#Dataframe 6-10
gtrends0610df16 <-  as.data.frame(matrix(ncol = 3, nrow = 261*5))
for (i in 1:lgeo2) {
  trendstest <- gtrends("Crop Insurance", geo = CBgeo.ISO610[i,2], time = "2016-01-01 2020-12-31")
  interest.ot <- trendstest$interest_over_time
  interest.ot2 <- interest.ot[c(1,2,4)]
  interestn <- nrow(interest.ot2)
  for (x in 1: interestn){
    rgt <- x +(261*(i-1))
    gtrends0610df16 [rgt,1] <- interest.ot2$geo[x]
    gtrends0610df16 [rgt,2] <- interest.ot2$hits[x]
    gtrends0610df16 [rgt,3] <- as.character(interest.ot2$date[x]) 
  }
}

gtrends0610df16
View(gtrends0610df16)
#Dataframe 1-5
gtrends0105df16 <- as.data.frame(matrix(ncol = 3, nrow = 261*5))
for (i in 1:lgeo1) {
  
  
  trendstest <- gtrends("Crop Insurance", geo = CBgeo.ISO15[i,2], time = "2016-01-01 2020-12-31")
  interest.ot <- trendstest$interest_over_time
  interest.ot2 <- interest.ot[c(1,2,4)]
  interestn <- nrow(interest.ot2)
  for (x in 1: interestn){
    rgt <- x +(261*(i-1))
    gtrends0105df16 [rgt,1] <- interest.ot2$geo[x]
    gtrends0105df16 [rgt,2] <- interest.ot2$hits[x]
    gtrends0105df16 [rgt,3] <- as.character(interest.ot2$date[x]) }}

gtrends0105df16


#Merging and Preparing of the Data Files

#Binding Variable Data


DF.PrecpTOT.16  <- rbind(DF.Precp.16, DF.Precp.17, DF.Precp.18, DF.Precp.19, DF.Precp.20)
DF.Tmax30TOT.16 <- rbind(DF.Tmax30.16,DF.Tmax30.17,DF.Tmax30.18,DF.Tmax30.19,DF.Tmax30.20)
DF.Tmax35TOT.16 <- rbind(DF.Tmax35.16,DF.Tmax35.17,DF.Tmax35.18,DF.Tmax35.19,DF.Tmax35.20)
DF.TminTOT.16   <- rbind(DF.Tmin.16, DF.Tmin.17, DF.Tmin.18, DF.Tmin.19, DF.Tmin.20)
View(DF.Tmax30TOT.16)
#Additional Temperature variable Data binding
DF.Tmax31TOT.16 <- rbind(DF.Tmax31.16,DF.Tmax31.17,DF.Tmax31.18,DF.Tmax31.19,DF.Tmax31.20)
DF.Tmax32TOT.16 <- rbind(DF.Tmax32.16,DF.Tmax32.17,DF.Tmax32.18,DF.Tmax32.19,DF.Tmax32.20)
DF.Tmax33TOT.16 <- rbind(DF.Tmax33.16,DF.Tmax33.17,DF.Tmax33.18,DF.Tmax33.19,DF.Tmax33.20)
View(DF.Tmax33TOT.16)
View(DF.Tmax30TOT.16)



#Set Names Fips, Value, Date

DF.PrecpTOT.16 <- setNames(DF.PrecpTOT.16, c("Fips", "Precp", "Date"))
DF.Tmax30TOT.16 <- setNames(DF.Tmax30TOT.16, c("Fips", "Tmax30", "Date"))
DF.Tmax35TOT.16 <- setNames(DF.Tmax35TOT.16, c("Fips", "Tmax35", "Date"))
DF.TminTOT.16 <- setNames(DF.TminTOT.16, c("Fips", "Tmin", "Date"))

#Set Names for additional temperature Data 

DF.Tmax31TOT.16 <- setNames(DF.Tmax31TOT.16, c("Fips", "Tmax31", "Date"))
DF.Tmax32TOT.16 <- setNames(DF.Tmax32TOT.16, c("Fips", "Tmax32", "Date"))
DF.Tmax33TOT.16 <- setNames(DF.Tmax33TOT.16, c("Fips", "Tmax33", "Date"))

#Merging
DF.Grid16Tot.1 <- merge(DF.PrecpTOT.16, DF.Tmax30TOT.16, by= c("Fips", "Date"))
DF.Grid16Tot.2 <- merge( DF.Grid16Tot.1, DF.Tmax35TOT.16, by= c("Fips", "Date"))
DF.Grid16Tot <- merge( DF.Grid16Tot.2,  DF.TminTOT.16, by= c("Fips", "Date"))

#Merging with additional Tmax data
DF.Grid16Tot3 <- merge(DF.Grid16Tot, DF.Tmax31TOT.16, by= c("Fips", "Date"))
DF.Grid16Tot4 <- merge(DF.Grid16Tot3,DF.Tmax32TOT.16, by= c("Fips", "Date"))
DF.Grid16.TT <- merge(DF.Grid16Tot4,DF.Tmax33TOT.16, by= c("Fips", "Date"))

#Assigning the week number for additional Temperature
DF.Grid16.TT$Number <- rep(1:261 , each = 7, length.out = 23751) 

#Adding Precp Square for additional Temp
DF.Grid16.TT$Precp2 <- DF.Grid16.TT$Precp^2

#Aggregation for the additional Temperature Frame 
DF.AGR.TT.GRID.16.Pre  <- aggregate(Precp~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Pre2 <- aggregate(Precp2~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmax30<- aggregate(Tmax30~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmax35<- aggregate(Tmax35~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmin  <- aggregate(Tmin~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmax31<- aggregate(Tmax31~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmax32<- aggregate(Tmax32~Fips+Number, DF.Grid16.TT, sum )
DF.AGR.TT.GRID.16.Tmax33<- aggregate(Tmax33~Fips+Number, DF.Grid16.TT, sum )

#MErging 2 for additional Temp
DF.AGR.TT.GRI.16.1 <-  merge(DF.AGR.TT.GRID.16.Pre,DF.AGR.TT.GRID.16.Pre2, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16.2<-  merge( DF.AGR.TT.GRI.16.1,DF.AGR.TT.GRID.16.Tmax30, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16.3<-  merge( DF.AGR.TT.GRI.16.2,DF.AGR.TT.GRID.16.Tmax35, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16.4<-  merge( DF.AGR.TT.GRI.16.3,DF.AGR.TT.GRID.16.Tmax31, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16.5<-  merge( DF.AGR.TT.GRI.16.4,DF.AGR.TT.GRID.16.Tmax32, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16.6<-  merge( DF.AGR.TT.GRI.16.5,DF.AGR.TT.GRID.16.Tmin, by= c("Number", "Fips"))
DF.AGR.TT.GRI.16<-  merge( DF.AGR.TT.GRI.16.6,DF.AGR.TT.GRID.16.Tmax33, by= c("Number", "Fips"))

#Creating a file to enable pairing of GTrends data and NOAA Data due to FIPS Code
 
DF.FIPS.COM.CB <- CBgeo.ISO
DF.FIPS.COM.CB$sub_code <- str_remove(DF.FIPS.COM.CB$sub_code, "US-") #adjusting the SUb value 
USSHPCBFI <- USSHPCB[c(3,6)] #to assign only the FIPS code and the SUB COde
DF.FIPS.COM.CB <- setNames(DF.FIPS.COM.CB, c("country_code","STUSPS","name")) #change naming to allow merging

DF.FIPS.Sub <- merge(DF.FIPS.COM.CB, USSHPCBFI, by = "STUSPS")
View(DF.FIPS.Sub)


DF.FIPS.Sub <- DF.FIPS.Sub %>% select(STUSPS,name, STATEFP)
DF.FIPS.Sub<- setNames(DF.FIPS.Sub, c("STUSPS","name", "Fips")) #Naming STATEFP Fips to fit to the Weather data
DF.FIPS.Sub 

#Adding STUSPS for additional temp gridded data
DF.AGR.TT.GRI.16S <- merge(DF.AGR.TT.GRI.16, DF.FIPS.Sub, by= "Fips")

#Bringing in GTrends 
DF.Gtrends.Tot16 <- rbind(gtrends0105df16, gtrends0610df16, gtrends1113df16)
DF.Gtrends.Tot16 <- setNames(DF.Gtrends.Tot16,c("STUSPS", "Hits", "Month"))
DF.Gtrends.Tot16$STUSPS <- str_remove(DF.Gtrends.Tot16$STUSPS, "US-")
DF.GtrendsTot16s <- sort(DF.Gtrends.Tot16$Month)
DF.Gtrends.Tot16$Year <- 1
DF.Gtrends.Tot16$Year <- format(as.Date(DF.Gtrends.Tot16$Month, format = "%Y-%m-%d"), format = "%Y")
DF.Gtrends.Tot16

#Assigning the Number
DF.Gtrends.Tot16$Number <-  rep(1:261,  length.out =3393)
View(DF.Gtrends.Tot16)

#Merging additional Temperature Data with gridded Data
DF.RegTT16<- merge(DF.Gtrends.Tot16, DF.AGR.TT.GRI.16S, by=c("STUSPS", "Number"))

#Regression Data file completed


