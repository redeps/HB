
yeasts <- data.frame(read.csv(file = "U:/R/HB/Yeasts.csv"))
#gsub("^[0-9]{2}","H",yeasts$Attentuation)
#gsub("^[0-9]{1}","H",yeasts$Attentuation)
#gsub("[^^.(.)]","HH",as.character(yeasts$Attentuation[[1]]))
#grep("^.(.)",as.character(yeasts$Attentuation[[1]]))

#yeasts$minatt <- as.integer(gsub("%.{1,}","",yeasts$Attentuation))
#yeasts$maxatt<- as.integer(gsub("%","",gsub(" ","",gsub("\\d{1,}%.-{1}","",yeasts$Attentuation))))
#yeasts$avgatt <- (yeasts$minatt+yeasts$maxatt) / 2
#yeasts$Tolerance <- as.integer(gsub("%", "", yeasts$Tolerance))
#yeasts$Temperature
#yeasts$mintemp <- as.integer(gsub("°.{1,}","",yeasts$Temperature))
#yeasts$maxtemp <- as.integer(gsub(" ","",gsub("°F","",gsub("\\d{1,}.°F.-","", yeasts$Temperature))))
#write.csv(yeasts, file = "S:/peder/training/R/HB/Yeasts.csv")

searchYeasts <- function(name){
  i <-grep(name, yeasts$Name,ignore.case=TRUE)
  if(nrow(yeasts[i,]) == 1){
    returnitem <- yeasts[i,]
  } else if(nrow(yeasts[i,]) > 1){
    lab <- readline(prompt = "Please enter yeast lab: ")
    returnitem <- searchLab(lab, yeasts[i,])
    if(nrow(returnitem) > 1){
      selection <- readline(prompt = paste0("Choose one of the following ", print((returnitem)[1:2]),": "))
      returnitem <- yeasts[selection,]
    }
  }
  return(returnitem)
}

searchYeastsSimple <- function(name){
  i <-grep(name, yeasts$Name,ignore.case=TRUE)
    returnitem <- yeasts[i,]
  return(returnitem)
}

searchLab <- function(name, df){
  i <-grep(name, df$Lab,ignore.case=TRUE)
  return(df[i,])
}

#searchYeasts("California ale")$maxatt
