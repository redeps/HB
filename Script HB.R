library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(shiny)
#install.packages("directlabels")
library(directlabels)
library(grid)
#install.packages("extrafont")
library(extrafont)
#install.packages("useful")
library(useful)
#font_import() # Import all fonts
#fonts() # Print list of all fonts
#install.packages("jpeg")
library(jpeg)

###################ENTER VALUES FOR FINAL COMPUTATION###############################

yeast <- "California Ale"
Hops<-c("Cascade","Willamette")
Hopweights <-list(55, 23)
Boilmin <- list(60)
Malts<-list("Cara","Barley")
Target_Batch_L <- 20
Grams_Grain <- 6000
Boil_time_Hr<-1
Target_Mash_Temp <- 67
Target_Batch_L <- 19
Boil_time_Hr <- 1
#celcius
Target_Mash_Temp<-64
Grain_Temp<-NULL
#percentage
yeast_attenuation<-NULL
brewhouse_efficiency=NULL
BeerName=NULL
BeerType=NULL

#Must equal lenght of Malts, and must add up to 100
Grainprops <-c(98,2)

#########################################################################################

##Creating kg to lb converter

gtolb <- function(g){
  lb <- g*.00220462
  return(lb)
}

##Creating Liter to Gallon converter

ltogal<-function(l){
  gal<-l*0.264172
  return(gal)
}

##Creating Gram to Oz converter

gtooz<-function(l){
  oz<-l*0.035274
  return(oz)
}

source("U:/R/HB/maltsdirectory.R")
source("U:/R/HB/hopsdirectory.R")
source("U:/R/HB/boilmashcalc.R")
source("U:/R/HB/gravitycolourcalc.R")
source("U:/R/HB/ibucalc.R")
source("U:/R/HB/yeastdirectory.R")
source("U:/R/HB/overallfunction.R")


#for yeasts:
# http://brewerwall.com/yeasts


#amountfunc(11, 4000, 1, 64)
#FGCalc(Malts, 4350, Grainprops, 19, "Ale yeast")
#OGBE <- as.numeric(FGCalc(Malts, 4350, Grainprops, 19)[[1]][1])
#IBUCalc(Hops, Hopweights, 11, OGBE, Boilmin)

#create R markdown html
#copy that into an r file into a new directory for each run
#markdown document will refer to directory's version of the numbers and plots




RunFGCalc(Malts, 4350, Grainprops, 19, Hops, Hopweights, Boilmin, 1, 64, yeast, 22,70)

load(file = "U:/R/HB/0606161339.Rdata")
suppressWarnings(returnlist[15])
suppressWarnings(returnlist[7])
suppressWarnings(returnlist[8])

#list <- as.list(RunFGCalc(Malts, 4000, Grainprops, 11, Hops, Hopweights, Boilmin, 1, 64))

#http://www.colorhexa.com/fae8aa
#make size of grainplot=grainprops
#fix name colour on axis label hopplot3
#make plot theme




stjerne <- data.frame(read.csv(file = "U:/R/HB/XY.csv"))
stjerne$Group <- factor(6)
south <- data.frame(read.csv(file = "U:/R/HB/XYsouth.csv"))
south$Group <- factor(7)
hopimg <- data.frame(read.csv(file = "U:/R/HB/XYhop.csv"))
hopimg$Group <- factor(7)
leaves <- data.frame(read.csv(file = "U:/R/HB/XYleaves.csv"))
leaves$Group <- as.factor(leaves$Group)
stem <- data.frame(read.csv(file = "U:/R/HB/XYstem.csv"))
stem$Group <- factor(10)
lineimg <- data.frame(read.csv(file = "U:/R/HB/XYline.csv"))
lineimg$Group <- factor(11)
brew <- data.frame(read.csv(file = "U:/R/HB/XYbrew.csv"))
brew$Group <- factor(7)
star <- data.frame(read.csv(file = "U:/R/HB/XYstar.csv"))
star$Group <- factor(11)

cbpalette <- c("#00a662","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette <- c(
  #leaf
  "#63f200",
  #stem
  "#f28f00",
  #design
  "#00f28f",
  #leaves
  "#00f28f",
  "#00dcf2",
  "#0063f2",
  "#1600f2",
  #text
  "#f20063"
  )



ggplot(data = south, aes(Xsouth,Ysouth, color = Group))+
  geom_point()+
  geom_point(data = hopimg, aes(Xhop,Yhop))+
  geom_point(data = leaves, aes(Xleaves,Yleaves))+
  geom_point(data = stem, aes(Xstem,Ystem))+
  geom_point(data = lineimg, aes(Xline,Yline))+
  geom_point(data = brew, aes(Xbrewnew,Ybrew))+
  geom_point(data = star, aes(Xstar,Ystar))+
  scale_colour_manual(values = c(
    #leaf
    "#63f200",
    #stem
    "#f28f00",
    #design
    "#00f28f",
    #leaves
    "#00f28f",
    "#00dcf2",
    "#0063f2",
    "#1600f2",
    #text
    "#f20063"
  ))






##################################################################################
##################################################################################
##################################################################################


library(grid)
vp <- viewport(x=0.5, y=0.5, width=0.9, height=0.9)
pushViewport(vp)

#grid.circle(x=0.6, y=0.7, r=0.3)
#grid.lines(c(.3,.5), c(.7,.1))
#grid.polygon(x=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.29,0.28,0.27,0.27,0.27,0.27,0.28,0.29,0.3, 0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68, 0.70, 0.8,0.81,0.82,0.83,0.83,0.83,0.83,0.82,0.81,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56), 
#             y=c(0.812,0.812,0.8115,0.811,0.8105,0.810,0.809,0.808,0.806,0.803,0.8,0.795,0.79,0.78,0.77,0.76,0.75,0.74,0.73,0.72,0.71,0.7,0.69,0.68,0.67,0.66,0.65, 0.2,0.195,0.193,0.190,0.188,0.187,0.186,0.186,0.186,0.187,0.188,0.190,0.190,0.193,0.195, 0.2, 0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.795,0.8,0.803,0.806,0.808,0.809,0.810,0.8105,0.811,0.8115,0.812,0.812))
#grid.polygon(x=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.32,0.34,0.36,0.38,0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68,0.7,0.72,0.74,0.76,0.78,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56),
#             y=c(0.812,0.812,0.8115,0.811,0.8105,0.810,0.809,0.808,0.806,0.803,0.8,0.795,0.79,0.78,0.77,0.76,0.75,0.74,0.735,0.73,0.725,0.72,0.716,0.713,0.711,0.710,0.709,0.708,0.707,0.707,0.707,0.707,0.708,0.709,0.710,0.711,0.713,0.716,0.72,0.725,0.73,0.735,0.74,0.75,0.76,0.77,0.78,0.79,0.795,0.8,0.803,0.806,0.808,0.809,0.810,0.8105,0.811,0.8115,0.812,0.812))


#grid.polygon(x=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.29,0.28,0.27,0.27,0.27,0.27,0.28,0.29,0.3, 0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68, 0.70, 0.8,0.81,0.82,0.83,0.83,0.83,0.83,0.82,0.81,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56), 
#             y=c(0.869,0.869,0.8685,0.868,0.8675,0.867,0.866,0.865,0.863,0.86,0.855,0.85,0.84,0.83,0.82,0.81,0.80,0.79,0.78,0.77,0.76,0.75,0.74,0.73,0.72,0.71,0.7,0.69,0.68,0.67,0.66,0.65, 0.2,0.195,0.193,0.190,0.188,0.187,0.186,0.186,0.186,0.187,0.188,0.190,0.190,0.193,0.195, 0.2, 0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.855,0.86,0.863,0.865,0.866,0.867,0.8675,0.868,0.8685,0.869,0.869,0.869))
#grid.polygon(x=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.32,0.34,0.36,0.38,0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68,0.7,0.72,0.74,0.76,0.78,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56),
#            y=c(0.869,0.869,0.8685,0.868,0.8675,0.867,0.866,0.865,0.863,0.86,0.855,0.79,0.78,0.77,0.76,0.75,0.74,0.735,0.73,0.725,0.72,0.716,0.713,0.711,0.710,0.709,0.708,0.707,0.707,0.707,0.707,0.708,0.709,0.710,0.711,0.713,0.716,0.72,0.725,0.73,0.735,0.74,0.75,0.76,0.77,0.78,0.79,0.855,0.86,0.863,0.865,0.866,0.867,0.8675,0.868,0.8685,0.869,0.869))


colours <-c("#F7D281","#EBA932","#DC8C00","#D07200","#BF5C00","#B24C00","#A63C00","#9A3000","#6D210A","#841900","#790F00","#730700","#6A0000","#620000","#853E3B","#550000","#500001","#823D2B","#440001","#400000","#3C0000","#370000","#330000","#300000","#2C0001","#290000","#260000","#230000","#220000","#1E0000","#1C0000","#1A0001","#180000","#160100","#140001","#140001","#120000","#100000","#0E0000","#0E0000")
SRMVAL <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)
SRM <- gravity_notes[3]

x=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.29,0.28,0.277,0.274,0.274,0.277,0.28,0.29,0.3, 0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68, 0.70, 0.8,0.81,0.82,0.823,0.824,0.824,0.823,0.82,0.81,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56)
y=c(0.869,0.869,0.8685,0.868,0.8675,0.867,0.866,0.865,0.863,0.86,0.855,0.85,0.84,0.83,0.82,0.81,0.80,0.79,0.78,0.77,0.76,0.75,0.74,0.73,0.72,0.71,0.7,0.69,0.68,0.67,0.66,0.65, 0.2,0.195,0.193,0.190,0.188,0.187,0.186,0.186,0.186,0.187,0.188,0.190,0.190,0.193,0.195, 0.2, 0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.855,0.86,0.863,0.865,0.866,0.867,0.8675,0.868,0.8685,0.869,0.869)
x2=c(0.54,0.52,0.50,0.48,0.46,0.44,0.42,0.40,0.38,0.36,0.34,0.32,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.32,0.34,0.36,0.38,0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.6,0.62,0.64,0.66,0.68,0.7,0.72,0.74,0.76,0.78,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.78,0.76,0.74,0.72,0.70,0.68,0.66,0.64,0.62,0.6,0.58,0.56)
y2=c(0.869,0.869,0.8685,0.868,0.8675,0.867,0.866,0.865,0.863,0.86,0.855,0.85,0.84,0.83,0.82,0.81,0.80,0.79,0.78,0.77,0.76,0.75,0.74,0.735,0.73,0.725,0.72,0.716,0.713,0.711,0.710,0.709,0.708,0.707,0.707,0.707,0.707,0.708,0.709,0.710,0.711,0.713,0.716,0.72,0.725,0.73,0.735,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.855,0.86,0.863,0.865,0.866,0.867,0.8675,0.868,0.8685,0.869,0.869)



y=c(0.869,0.869,0.8685,0.868,0.8675,0.867,0.866,0.865,0.863,0.86,0.855,0.85,0.84,0.83,0.82,0.81,0.80,0.79,0.78,0.77,0.76,0.75,0.74,0.73,0.72,0.71,0.7,0.69,0.68,0.67,0.66,0.65, 0.2,0.188,0.185,0.182,0.179,0.177,0.176,0.1755,0.1755,0.176,0.1787,0.179,0.182,0.185,0.188,0.2, 0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,0.84,0.85,0.855,0.86,0.863,0.865,0.866,0.867,0.8675,0.868,0.8685,0.869,0.869)

glass <-  gList(grid.polygon(x=x, 
                             y=y, gp=gpar(fill="#BF5C00",col="grey",lwd=1.5)),
                grid.polygon(x=x2,
                             y=y2,gp=gpar(fill="#EEE4D3",col="#EEE4D3", lwd=0))
)
grid.draw(glass)



#LEFT SIDE LINE

grid.lines(c(0.33,0.34),c(0.850,0.853), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.32,0.33),c(0.848,0.850), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.31,0.32),c(0.843,0.848), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.30,0.31),c(0.84,0.843), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.30,0.32),c(0.84,0.83), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.32,0.34),c(0.83,0.82), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.34,0.36),c(0.82,0.815), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.36,0.38),c(0.815,0.81), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.38,0.40),c(0.81,0.807), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.30,0.30),c(0.83,0.75), gp=gpar(col="white", lwd=5))
grid.lines(c(0.30,0.30),c(0.84,0.74), gp=gpar(col="grey", lwd=2))


#RIGHT SIDE LINE
grid.lines(c(0.8,0.8),c(0.84,0.75), gp=gpar(col="white", lwd=5))
grid.lines(c(0.8,0.8),c(0.84,0.74), gp=gpar(col="grey", lwd=2))
grid.lines(c(0.8,0.78),c(0.84,0.83), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.78,0.76),c(0.83,0.82), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.76,0.74),c(0.82,0.815), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.74,0.72),c(0.815,0.81), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.72,0.70),c(0.81,0.807), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.70,0.68),c(0.807,0.805), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.68,0.66),c(0.805,0.804), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.66,0.64),c(0.804,0.8035), gp=gpar(col="grey", lwd=3))

grid.draw(glass)

#LEFT SIDE
grid.lines(c(0.3,0.29),c(0.74,0.73 ), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.29,0.28),c(0.73,0.72 ), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.28,0.277),c(0.72,0.71), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.277,0.275),c(0.71,0.70), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.275,0.277),c(0.70,0.69), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.277,0.277 ),c(0.69,0.68), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.277,0.28 ),c(0.68,0.67), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.28,0.29 ),c(0.67,0.66), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.29,0.3 ),c(0.66,0.65), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.3,0.335 ),c(0.65,0.5), gp=gpar(col="grey", lwd=2.5))
grid.lines(c(0.3,0.35 ),c(0.65,0.425), gp=gpar(col="grey", lwd=2))


#RIGHT SIDE

grid.lines(c(0.8,0.81),c(0.74,0.73 ), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.81,0.82),c(0.73,0.72 ), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.82,0.823),c(0.72,0.71), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.823,0.824),c(0.71,0.70), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.824,0.824),c(0.70,0.69), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.824,0.824 ),c(0.69,0.68), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.824,0.823 ),c(0.68,0.67), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.823,0.81 ),c(0.67,0.66), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.81,0.80 ),c(0.66,0.65), gp=gpar(col="grey", lwd=3))
grid.lines(c(0.8,0.765 ),c(0.65,0.5), gp=gpar(col="grey", lwd=2.5))
grid.lines(c(0.8,0.7 ),c(0.65,0.2), gp=gpar(col="grey", lwd=2))

#BOTTOM
grid.polygon(x= c(0.286,0.287,0.288,0.289,0.291,0.295,0.297,0.3,0.2,0.195,0.193,0.190,0.188,0.187,0.186,0.186,0.186,0.187,0.188,0.190,0.190,0.193,0.195, 0.2,0.3,0.297,0.295,0.291,0.289,0.288,0.287,0.286),
             y=c())


