# this code gets sourced from this file, 
# and creates the "dat" object, which is then used in the main script
library(tidyverse)
library(dplyr)
library(tidyr)
#rawdat<-as.data.frame(read.csv("data/Speel_sockeye_1986_onward.csv",header=T)) 
rawdat<-as.data.frame(read.csv("data/Speel_sockeye.csv",header=T)) 
nyrs<-as.numeric(length(rawdat$year)) 
fyr<-min(rawdat$year) 
lyr<-max(rawdat$year) 
a.max<-6
a.min<-4
A<-3 

#data clean---- 
year <- as.numeric(as.character(rawdat$year)) 
w <- as.numeric(as.character(rawdat$w)) 
cv.w <- as.numeric(as.character(rawdat$cv.w)) 
h.b <- as.numeric(as.character(rawdat$h.b)) #harvest below weir
cv.hb <- as.numeric(as.character(rawdat$cv.hb))
x<-as.matrix(rawdat[,substr(colnames(rawdat), 1,1)=="x"])#age comp count data matrix ; these need to be integers 
colnames(x)<-NULL 
n.a<-rowSums(x) # age comp sample sizes 

dat=list(Y = nyrs, A=A, a.min=a.min, a.max=a.max, 
x=x, w=w, cv.w=cv.w, h.b=h.b,
n.a=n.a, cv.hb=cv.hb)



