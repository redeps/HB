



###################ENTER VALUES FOR FINAL COMPUTATION###############################



yeast <- "California Ale V"
Hops<-c("Citra")
Hopweights <-list(207)
Boilmin <- list(60)
Malts<-list("Maris Otter","Crystal Malt 60","Dark Crystal Malt")
Target_Batch_L <- 20
Grams_Grain <- 7348
Boil_time_Hr<-1
Target_Mash_Temp <- 64
#celcius
Grain_Temp<-NULL
#percentage
brewhouse_efficiency=NULL
BeerName="Citra Vestkyst"
BeerType="West coast IPA"

#Must equal lenght of Malts, and must add up to 100
Grainprops <-c(92,4,4)

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

#setwd("S:/Peder/training/R/HB")
#getwd()

source(paste0(getwd(),"/maltsdirectory.R"))
source(paste0(getwd(),"/hopsdirectory.R"))
source(paste0(getwd(),"/boilmashcalc.R"))
source(paste0(getwd(),"/gravitycolourcalc.R"))
source(paste0(getwd(),"/ibucalc.R"))
source(paste0(getwd(),"/yeastdirectory.R"))
source(paste0(getwd(),"/overallfunction.R"))
