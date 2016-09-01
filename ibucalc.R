### Creating IBU calculator
#Needs list of hops, list of hop weights,  Target batch in litres, OG,and list of boil times for each hop

#The first step is to calculate the Alpha Acid Units (AAUs).
#AAU = Weight (oz) x % Alpha Acids (whole number)
#Hops<-list("Cascade")
#Must equal lenght of Malts, and must add up to 100
#Hopweights <-list(50)
#Boilmin <- list(60)
#OGBE <- (FGCalc(Malts, 4350, Grainprops, 19)[1])
divHoplist <- function(Hops, Hopweights){
  NewHops <- c()
  NewHopweights <- c()
  NewBoilmins <- c()
  for(hop in Hops){
    answer <- as.numeric(readline(prompt = paste("How many times will you be adding",hop,"?")))
    hopname <- c()
    mins <- c()
    wgts <- c()
    for(addition in 1:answer){
      minute <- as.numeric(readline(prompt = paste("Enter minute mark for addition number",addition,":")))
      weight <- as.numeric(readline(prompt = paste("Enter weight for addition number",addition,":")))
      hopname[addition] <- hop
      mins[addition] <- minute
      wgts[addition] <- weight
    }
    NewHops <- append(NewHops,hopname)
    NewHopweights <- append(NewHopweights, wgts)
    NewBoilmins <- append(NewBoilmins, mins)
  }
  return(list(NewHops, NewHopweights,NewBoilmins))
}




ShinyIBUCalc <- function(Hops, Hopweights, Target_Batch_L, OGBE, Boilmin){
  Bigness_factor <- 1.65 * (0.000125**((OGBE/1000)-1))
  Hops$Boil_time_factor <- 0
  #http://realbeer.com/hops/research.html
  #I'd suggest fiddling with 4.15 if necessary to match your system; only play with the other three if you like to muck around. I make no guarantees if you do.
  for(row in 1:length(Boilmin)){
          euler <-  (exp(1))
          time <- (-0.04)*as.numeric(Boilmin[row])
          expon <- euler^time
          Hops[row,]$Boil_time_factor <- (1-expon)/4.15
  }
  Hops$Hopweights <- Hopweights
  
  Hops$decimal_alpha_acid_util <- Bigness_factor * Hops$Boil_time_factor

  Hops$AAconc<-(as.numeric(Hops$Hopweights) * (as.numeric(Hops$approxalpha)/100) * 1000)/ Target_Batch_L

  Hops$IBU <- (Hops$AAconc * as.numeric(Hops$decimal_alpha_acid_util))

  return(Hops)
}


IBUCalc <- function(Hops, Hopweights, Target_Batch_L, OGBE, Boilmin){
  Newvalues <- divHoplist(Hops, Hopweights)
  Hops <- Newvalues[[1]]
  Hopweights <- Newvalues[[2]]
  Boilmin <- Newvalues[[3]]
  Weightframe2 <- data.frame(data.table(Hops, Hopweights,Boilmin))
  Bigness_factor <- 1.65 * (0.000125**((OGBE/1000)-1))
  #print(Bigness_factor)
  #I'd suggest fiddling with 4.15 if necessary to match your system; only play with the other three if you like to muck around. I make no guarantees if you do.
  for(row in 1:length(Weightframe2$Boilmin)){
    euler <-  (exp(1))
    time <- (-0.04)*as.numeric(Weightframe2$Boilmin)[row]
    expon <- euler^time
    Boil_time_factor <- (1-expon)/4.15
    #print(Boil_time_factor)
    Weightframe2$Boil_time_factor[row]<-Boil_time_factor
  }
  Weightframe2$decimal_alpha_acid_util <- Bigness_factor * Weightframe2$Boil_time_factor
  #print(Weightframe2)
  Hop2<-data.frame(data.table(Hops))
  for(hop in 1:length(Hops)){
    Hop<-searchhops(Hops[hop])
    length<-length(Hop$Row.names)
    if(length>1){
      print(paste0("Is this the right hop: ", Hop$Name[1], "?"))
      print(paste0("If not, these are the alternatives: ", Hop$Name[2:length]))
    }
    #if(length==1){
    #  print(paste0("Hop flavour description: ", Hop$Flavor.Description))
    #}
    #print(Hop)
    AA<-Hop$approxalpha[1]
    Name<-Hop$Name[1]
    AAH<-Hop$Alpha.Acid.High[1]
    AAs<-Hop$Alpha.Acids[1]
    Co<- Hop$cohumulone[1]
    Fa<-Hop$Farneseneplot[1]
    Ca<-Hop$Caryophylleneplot[1]
    Hu<-Hop$Humuleneplot[1]
    My<-Hop$MyrcenePER100G[1]
    
    Weightframe2$AA[hop]<-(AA)
    Hop2$Name[hop]<-Name
    Hop2$Alpha.Acid.High[hop]<-(AAH)
    Hop2$Alpha.Acids[hop]<-(AAs)
    Hop2$cohumulone[hop]<- (Co)
    Hop2$Farneseneplot[hop]<- (Fa)
    Hop2$Caryophylleneplot[hop]<- (Ca)
    Hop2$Humuleneplot[hop]<- (Hu)
    Hop2$MyrcenePER100G[hop]<- (My)
    
  }
  Weightframe2$AAconc<-(as.numeric(Weightframe2$Hopweights) * (Weightframe2$AA/100) * 1000)/ Target_Batch_L
  Weightframe2$IBU <- (Weightframe2$AAconc * Weightframe2$decimal_alpha_acid_util)
  
  print(paste0("Expected IBUs: ", round(sum(Weightframe2$IBU))))
  return(Weightframe2)
}

#IBUCalc(Hops, Hopweights, 11, OGBE, Boilmin)




### End creating IBU calculator
