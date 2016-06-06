
#### Creating Boil and Mash calculations
#mydata[2]<-gsub("[^[:alnum:][:blank:]]", "", mydata[2])

amountfunc <- function(Target_Batch_L, Grams_Grain, Boil_time_Hr, Target_Mash_Temp, Grain_Temp=NULL){
  #Grain_Temp auto set to 22C
  #Test to see if this works
  if(is.null(Grain_Temp)){
    Grain_Temp <- readline(prompt = "Enter Grain_Temp if different from default, 22, otherwise hit enter: ")
    Grain_Temp<- ifelse(Grain_Temp != "", as.numeric(Grain_Temp), 22 )
  }
  Trub_loss <- as.numeric((1/10)*Target_Batch_L)
  Equipment_Loss <- as.numeric((1/5)*Target_Batch_L)
  Mash_Thickness <- as.numeric(1.2/1)
  #Mash Thickness- most homebrewers know this as a ration of quarts per pound, often 1.25 quarts of water per pound of grain (1.2 liters)https://byo.com/stories/item/1110-managing-mash-thickness.
  Heat_Capacity_Water <- 4.2
  Grain_Abs_Const <- (1.56/1000)
  Wort_Shrinkage <- .04
  Perc_Boiloff_HR <- .1
  Strike_Volume <- round(as.numeric(Mash_Thickness * (Grams_Grain/453.592)),3)
  Water_Loss_Mash <- as.numeric(Strike_Volume-(Grain_Abs_Const*Grams_Grain))
  Water_Loss_Boil <- as.numeric(Target_Batch_L*(Perc_Boiloff_HR*Boil_time_Hr)) + (Target_Batch_L*Wort_Shrinkage) + Trub_loss + Equipment_Loss
  Pre_Sparge_Volume <- as.numeric(Strike_Volume - Water_Loss_Mash)
  Sparge_Volume <- round(as.numeric(Target_Batch_L - Pre_Sparge_Volume),3)
  print(paste0("Strike volume (L): ", Strike_Volume))
  print(paste0("Sparge volume (L): ", Sparge_Volume))
  Total_Water <- Strike_Volume + Sparge_Volume
  print(paste0("Total volume (L): ", Total_Water))
  Strike_Tempfunc <-function(Target_Mash_Temp,Grain_Temp){
    imperial_mash_thickness <- (1.25/1)
    A <- (0.2/as.numeric(imperial_mash_thickness))
    B <- as.numeric(Target_Mash_Temp)*(9/5)+32.0
    C <- as.numeric(Grain_Temp) *(9/5)+32.0
    D <- B-C
    E <- A*D
    Strike_Temp <- ((E+B) -32)*(5/9)
    return(Strike_Temp)
  }
  Strike_Temp <-Strike_Tempfunc(Target_Mash_Temp,Grain_Temp)
  print(paste0("Strike temp (C): ", Strike_Temp))
  recipe_notes<-c(Strike_Volume, Sparge_Volume, Total_Water, Strike_Temp, Target_Batch_L, Target_Mash_Temp)
  return(recipe_notes)
}

#amountfunc(11, 4000, 1, 64)


#### End creating Boil and Mash calculations
