#Regression model
library(dplyr)
library(fixest)

#Regression base model with termperature threshold 30 degree Celcius
RegressionWG.162 <-feols(Hits ~ Tmin+Precp+Tmax30+Precp2 | Number+STUSPS,  DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionWG.162)

#Robustness Tests

#Creating half subsetss
ListN <- c("27", "38", "46", "55", "26", "31", "19") 
DF.RegTOT1SUBN<-  DF.RegTT16 %>% 
  filter(Fips %in% ListN)
View(DF.RegTOT1SUBN)
#ND 27,SD 38, Mineesota 46, Wisconsin 55, Michigan 26, Nebraska 31,  Iwoa, 19

ListS <- c("21", "17", "29", "20", "18","39") 
DF.RegTOT1SUBS<-  DF.RegTT16 %>% 
  filter(Fips %in% ListS)
View(DF.RegTOT1SUBS)
#Kentucky 21, Illinois 17, Missouri 29 Kansas20, Indiana 18, Ohio 39 

ListE <- c("55", "17", "18", "21", "26", "39") #Wisconsin 55, Illinois 17, Indiana 18, Kentucky 21, Michigan 26, Ohio 39
DF.RegTOT1SUBE<-  DF.RegTT16 %>% 
  filter(Fips %in% ListE)
View(DF.RegTOT1SUBE)

ListW <- c("38", "46", "31", "20", "27", "19", "29") #ND 38, SD 46, Nebraska 31, Kansas 20 , Minnesota 27, Iowa 19, Missouri 29
DF.RegTOT1SUBW<-  DF.RegTT16 %>% 
  filter(Fips %in% ListW)
View(DF.RegTOT1SUBW)



#RObustness Check northern half
DF.RegSUBN <-feols(Hits ~ Tmin+Precp+Tmax30+Precp2 |Number+STUSPS, DF.RegTOT1SUBN, cluster = c("Number","STUSPS"))
summary(DF.RegSUBN)


#RObustness Check southern half
DF.RegSUBS <-feols(Hits ~ Tmin+Precp+Tmax30+Precp2 |Number+STUSPS, DF.RegTOT1SUBS, cluster = c("Number","STUSPS"))
summary(DF.RegSUBS)


#Robustness  Check  western half
DF.RegSUBW <-feols(Hits ~ Tmin+Precp+Tmax30+Precp2 |Number+STUSPS, DF.RegTOT1SUBW, cluster = c("Number","STUSPS")) #Unfamiliar problem because of the clustering
summary(DF.RegSUBW)


#Robustness Check eastern half
DF.RegSUBE <-feols(Hits ~ Tmin+Precp+Tmax30+Precp2 |Number+STUSPS, DF.RegTOT1SUBE, cluster = c("Number","STUSPS"))
summary(DF.RegSUBE)



#Temperature comparison calculation
#Temp 30
RegressionTT.30 <-feols(Hits ~ Tmin++Tmax30+Precp+Precp2 | Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionTT.30)
coefplot(RegressionTT.30)
#Temp 31
RegressionTT.31 <-feols(Hits ~ Tmin+Tmax31+Precp+Precp2 | Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionTT.31)
coefplot(RegressionTT.31)
#Temp 32
RegressionTT.32 <-feols(Hits ~ Tmin+Tmax32+Precp+Precp2 | Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionTT.32)
coefplot(RegressionTT.32)
#Temp 33
RegressionTT.33 <-feols(Hits ~ Tmin+Tmax33+Precp+Precp2 | Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionTT.33)

#Temp 35
RegressionTT.35 <-feols(Hits ~ Tmin+Tmax35+Precp+Precp2 | Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS"))
summary(RegressionTT.35)

Tempcomparison <- etable(RegressionTT.30,RegressionTT.31,RegressionTT.32,RegressionTT.33,RegressionTT.35)
Tempcomparison


#test multiple estimation

RegressionTT.MT <-feols(Hits ~ Tmin+sw(Tmax30, Tmax31, Tmax32, Tmax33, Tmax35)+Precp+Precp2| Number+STUSPS, DF.RegTT16, cluster = c("Number","STUSPS")) #Works 
summary(RegressionTT.MT)

ReGLISTT <- as.list(RegressionTT.MT)
coefplot(ReGLISTT)

#RObustness Chekcs with for Summer (growing) periods, and winter period

#Creting Half year Subsets: 

DF.RegTT16$Monat <- format(as.Date(DF.RegTT16$Month, format = "%Y-%m-%d"), format = "%m")
DF.RegTT16$Monat <- as.numeric(DF.RegTT16$Monat)

DF.RegTT16W <- filter(DF.RegTT16, DF.RegTT16$Monat %in% c(11,12,1,2))
DF.RegTT16S <- filter(DF.RegTT16, DF.RegTT16$Monat %in% c(3,4, 5,6,7,8,9,10))


#Robustness for the base model with half years
RegressionTT.30S3 <-feols(Hits ~Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.RegTT16S,  cluster = c("Number","STUSPS")) 
RegressionTT.30W3 <-feols(Hits ~Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.RegTT16W,  cluster = c("Number","STUSPS"))
summary(RegressionTT.30S3)
summary(RegressionTT.30W3)


#Robustness Check splitting Precipitation into quartiles #Not used for the Thesis

DF.Precp.SUB1 <- filter(DF.RegTT16, between( DF.RegTT16$Precp,0,44015))
DF.Precp.SUB2 <- filter(DF.RegTT16, between( DF.RegTT16$Precp,44016,118834))
DF.Precp.SUB3 <- filter(DF.RegTT16, between( DF.RegTT16$Precp,118834,237586))
DF.Precp.SUB4 <- filter(DF.RegTT16, between( DF.RegTT16$Precp,237586,1731907))
View(DF.Precp.SUB2)

RegressionPS1.30 <-feols(Hits ~ Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB1, cluster = c("Number","STUSPS"))
RegressionPS2.30 <-feols(Hits ~ Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB2, cluster = c("Number","STUSPS"))
RegressionPS3.30 <-feols(Hits ~ Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB3, cluster = c("Number","STUSPS"))
RegressionPS4.30 <-feols(Hits ~ Tmin+Tmax30+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB4, cluster = c("Number","STUSPS"))

summary(RegressionPS1.30)
summary(RegressionPS2.30)
summary(RegressionPS3.30)
summary(RegressionPS4.30)
RegressionPS1.31 <-feols(Hits ~ Tmin+Tmax31+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB1, cluster = c("Number","STUSPS"))
RegressionPS2.31 <-feols(Hits ~ Tmin+Tmax31+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB2, cluster = c("Number","STUSPS"))
RegressionPS3.31 <-feols(Hits ~ Tmin+Tmax31+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB3, cluster = c("Number","STUSPS"))
RegressionPS4.31 <-feols(Hits ~ Tmin+Tmax31+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB4, cluster = c("Number","STUSPS"))
summary(RegressionPS1.31)
summary(RegressionPS2.31)
summary(RegressionPS3.31)
summary(RegressionPS4.31)

RegressionPS1.32 <-feols(Hits ~ Tmin+Tmax32+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB1, cluster = c("Number","STUSPS"))
RegressionPS2.32 <-feols(Hits ~ Tmin+Tmax32+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB2, cluster = c("Number","STUSPS"))
RegressionPS3.32 <-feols(Hits ~ Tmin+Tmax32+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB3, cluster = c("Number","STUSPS"))
RegressionPS4.32 <-feols(Hits ~ Tmin+Tmax32+Precp+Precp2 | Number+STUSPS, DF.Precp.SUB4, cluster = c("Number","STUSPS"))
summary(RegressionPS1.32)
summary(RegressionPS2.32)
summary(RegressionPS3.32)
summary(RegressionPS4.32)