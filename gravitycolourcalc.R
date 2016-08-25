

#### Creating Gravity and colour calculator
#Malts<-list("Maris Otter","Caramunich","Caramunich Malt I")
#Must equal lenght of Malts, and must add up to 100
#Grainprops <-c(50,25,25)


getGrainprops <- function(Malts){
  Grainprops <- c()
  for(item in Malts){
    Grainprop <- readline(prompt = paste("Enter percentage (ie 45)",item,":"))
    Grainprop <- as.numeric(Grainprop)
    Grainprops <- c(Grainprops,Grainprop)
  }
  summation <- (sum(as.numeric(Grainprops)))
  if(summation != 100){
    print("Grainprops must equal 100")
    getGrainprops(Malts)
  } else {
    return(Grainprops) 
    break
  }
}

getMalts <- function(Malt,length){
  maltanswer <- toupper(readline(prompt = (paste0("Is this the right malt: ", Malt$Malt[1], "? Y/N: "))))
  if(maltanswer == "Y" | maltanswer == ""){
    Malt <- Malt[1,]
  } else {
    alternatives <- c()
    for(item in 1: length(Malt$Malt[2:length])){
      alternatives <- paste(alternatives, paste0("[",item,"] ",Malt$Malt[item+1]))
    }
    maltanswer2 <- as.numeric(readline(prompt = paste0("If not, these are the alternatives: ", alternatives,". Please enter the number corresponding to the Malt: ")))
    Malt <- Malt[(maltanswer2+1),]
  }
  return(Malt)
}

getMaltsShiny <- function(Malts){
  maltlist <- c()
  for(malt in 1:length(Malts)){
    if(malt == 1){
      Malt <- searchmalts(Malts[malt])
    } else
      Malt <- rbind(Malt, searchmalts(Malts[malt]))
  }
  return(Malt)
}

checkDec <- function(brewhouse_efficiency){
  if(brewhouse_efficiency != "" & brewhouse_efficiency >= 1){
    brewhouse_efficiency <- readline(prompt = "Enter brewhouse_efficiency as a decimal (needs to be 1 or lower): ")
    checkDec(brewhouse_efficiency)
  } else {
    return(brewhouse_efficiency)
  }
}


FGCalc <- function(Malts,Grams_Grain, Grainprops,Target_Batch_L, yeast, brewhouse_efficiency=NULL){
  if(sum(Grainprops) != 100){
    Grainprops <- getGrainprops(Malts)
  }
  Target_Batch_gal <- ltogal(Target_Batch_L)
  #print(Target_Batch_gal)
  #Test to see if the two following work:
  yeastinfo <- searchYeasts(yeast)
  yeast_attenuation <- ifelse(nrow(yeastinfo) == 1, yeastinfo$avgatt, NULL)
  if(is.null(yeast_attenuation)){
    yeast_attenuation <- readline(prompt = "Enter yeast attenuation (as percentage) to see expected FG, or hit enter to skip this step: ")
    yeast_attenuation<- ifelse(!is.null(yeast_attenuation), as.numeric(yeast_attenuation), NA )
  }
  
  if(is.null(brewhouse_efficiency)){
    brewhouse_efficiency <- readline(prompt = "Enter brewhouse_efficiency if different from default, 0.72 (as decimal), or hit Enter to keep default: ")
    brewhouse_efficiency <- checkDec(brewhouse_efficiency)
    brewhouse_efficiency<- ifelse(brewhouse_efficiency != "", as.numeric(brewhouse_efficiency), 0.72 )
  }
  Grams_Grain<-gtolb(Grams_Grain)

  Weightframe <- data.frame(data.table(Malts, Grainprops))
  Weightframe$Grainwgt <- (Grams_Grain*Weightframe$Grainprops/100)
  #print(Weightframe$Grainwg)
  for(malt in 1:length(Malts)){
    Malt<-searchmalts(Malts[malt])
    length<-length(Malt$Malt)
    #Test to see if the following works:
    if(length>1){
      Malt <- getMalts(Malt, length)
    }
    G<-Malt$G
    #print(Malt)
    L<-Malt$L 
    #This corresponds to the contribution that a pound of grain or extract will add if dissolved in a gallon of water. The maximum potential is approximately 1.046 which would be a pound of pure sugar in a gallon of water.
    #"potential" contribution
    potcont<-(G)-1000
    Weightframe$Malts[malt] <- Malt[1]
    Weightframe$potcont[malt]<-(potcont)
    Weightframe$colourcont[malt] <- ((L)*Weightframe$Grainwgt[malt])/Target_Batch_gal

    Weightframe$description[malt] <- Malt$Decription
    Weightframe$type <- Malt$Type
  }
  MCU<- round(sum(Weightframe$colourcont))
  SRM<- round(1.4922 * (MCU **0.6859))
  print(paste0("Colour:: Expected SRM: ", SRM, ", Expected MCU: ", MCU))
  Weightframe$totpot<-((Weightframe$Grainwgt*Weightframe$potcont)*brewhouse_efficiency)
  OG100gal <- round(sum((Weightframe$Grainwgt*Weightframe$potcont))/Target_Batch_gal)
  OG100 <- round(sum((Weightframe$Grainwgt*Weightframe$potcont))/Target_Batch_gal)+1000
  OGBEgal <- round((sum(Weightframe$totpot))/Target_Batch_gal)
  OGBE <- (round((sum(Weightframe$totpot))/Target_Batch_gal)+1000)
  #print(paste0("OG at 100% efficiency: ", OG100gal, " per gallon"))
  #print(paste0("OG at 100% efficiency: ", OG100))
  print(paste0("OG at brewhouse efficiency: ", OGBEgal," per gallon"))
  print(paste0("OG at brewhouse efficiency: ",OGBE))
  if(!is.null(yeast_attenuation)){
    EFG <- round(OGBEgal*(1-(yeast_attenuation/100))+1000)
    EABV <- ((OGBE - EFG)*131)/1000
    print(paste0("Expected FG: ", EFG))
    print(paste0("Expected ABV: ", EABV))
    gravity_notes<-c(OGBE,OGBEgal,SRM,MCU,EFG,EABV)
  }
  if(is.null(yeast_attenuation)){
    gravity_notes<-c(OGBE,OGBEgal,SRM,MCU,"","")
  }
  gravity_notes <- list(gravity_notes,Weightframe, yeastinfo)
  return(gravity_notes)
}

#FGCalc(Malts, 4350, Grainprops, 19)



#In practice the EBC color is approximately 1.97 times the SRM color. (EBC = 1.97 * SRM) 
#MCU = (Weight of grain in lbs) * (Color of grain in degrees lovibond) / (volume in gallons)
#A first iteration at estimating beer color involved simply calculating the Malt Color Units (MCUs) of a recipe.
#For multiple grain additions, you can simply calculate the MCU for each addition and add them together. MCU provides a good estimate of SRM color for light beers, but starts to diverge as beer color exceeds 6-8 SRM, because light absorbance is logarithmic and not linear. For a more accurate estimate that holds for darker beers up to about 50 SRM, we turn to the Morey equation:
#SRM color = 1.4922 * (MCU ** 0.6859)



#### End Creating Gravity and colour calculator
