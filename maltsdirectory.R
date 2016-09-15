###Creating searchable malt directory

#setwd('S:\\Peder\\training\\Python')
mydata2 <- data.frame(read.csv("Malts.csv",sep=",",header=FALSE),stringsAsFactors=FALSE)
mydata2[] <- lapply(mydata2, as.character)
names <-mydata2[23,]
colnames(mydata2)<-as.character(names[1,])
Type <-list()

for(i in 1:22){
  a<-c(mydata2$Malt[1])
  Type[[i]]<-a
}
Skip<-list()
for(i in 1:2){
  a<-c('na')
  Skip[[i]]<-a
}
Type2<-list()
for(i in 1:7){
  a<-c(mydata2$Malt[24])
  Type2[[i]]<-a
}
Type3<-list()
for(i in 1:11){
  a<-c(mydata2$Malt[33])
  Type3[[i]]<-a
}
Type4<-list()
for(i in 1:16){
  a<-c(mydata2$Malt[46])
  Type4[[i]]<-a
}
Type5<-list()
for(i in 1:18){
  a<-c(mydata2$Malt[64])
  Type5[[i]]<-a
}
Type6<-list()
for(i in 1:21){
  a<-c(mydata2$Malt[84])
  Type6[[i]]<-a
}
name<-c(Type,Skip,Type2,Skip,Type3,Skip,Type4,Skip,Type5,Skip,Type6)
mydata2$Type<-name
mydata2$Type[1]<-"na"
mydata2$Type <- ifelse(mydata2$Type == "na",NA,mydata2$Type)


#fixing unknown character (degree)

mydata2$L <- ifelse(mydata2$L == "",NA,mydata2$L)
list <-(strsplit(mydata2$L, "-"))
df <- do.call(rbind.data.frame, list)
colnames(df) <- c("Low","High")
df$High <-gsub("[^[:alnum:]///' ]", "", df$High)
df$Low <-gsub("[^[:alnum:]///' ]", "", df$Low)
df$Low<-as.numeric(as.character(df$Low))
df$High<-as.numeric(as.character(df$High))
df$Average <- (df$Low+df$High)/2
mydata2$L<-df$Average
mydata2$G <- ifelse(mydata2$G == "",NA,mydata2$G)
list <-(strsplit(mydata2$G, "-"))
df <- do.call(rbind.data.frame, list)
colnames(df) <- c("Low","High")
df$Low <-gsub("[^[:alnum:]///' ]", "", df$Low)
df$Low<-as.numeric(as.character(df$Low))
mydata2$G <- df$Low
mydata2$Decription<-gsub("[^[:alnum:]///' ]", "", mydata2$Decription)
#end fixing unknown character (degree)
colnames(mydata2) <- gsub('Decription','Description',colnames(mydata2))


MaltDirectory<-data.frame(subset(mydata2, !is.na(Type)))
MaltDirectory$Malt <- paste(as.character(MaltDirectory$Malt),as.character(MaltDirectory$L))


LaQuebecoiseMalt<-as.data.frame(list("La Quebecoise Malt",2.9,1035,"Canada Malting Co's Quebecoise Malt is 100% grown and malted in Quebec. It is a pale style malt with a slightly higher color than a basic 2 row. The grain is selected from various regions in Quebec and is then malted at their Montreal Plant","Canadian Grains"))
list<-names(MaltDirectory)
names(LaQuebecoiseMalt)<-list

MaltDirectory<-rbind(MaltDirectory,LaQuebecoiseMalt)


addRowM <- function(list){
  if(length(list) == length(MaltDirectory[nrow(MaltDirectory),])){
    MaltDirectory[nrow(MaltDirectory)+1,] <- list
    row.names(MaltDirectory) <- 1:nrow(MaltDirectory)
  } 
  return(MaltDirectory)
}

MaltDirectory <- addRowM(list("Weyermann Premium Pilsner",1.4,1037,"Perfect base for extra pale lagers, substantial mouthfeel, head, use up to 100%, pilsners, all lagers, low-alchohol beers, belgian beers","",NA))

searchmalts <- function(name){
  i <-grep(name, MaltDirectory$Malt,ignore.case=TRUE)
  return(MaltDirectory[i,])
}
#eg
#searchmalts("Cara")

searchflav <- function(flavour){
  i <-grep(flavour, MaltDirectory$Decription,ignore.case=TRUE)
  return(MaltDirectory[i, ])
}
#eg
#searchflav("coffee")

searchtype <- function(type){
  i <-grep(type, MaltDirectory$Decription,ignore.case=TRUE)
  return(MaltDirectory$Malt[i])
}
#eg
#searchtype("stout")

MaltDirectory$group <- 2


## creating grouping function for plot
groupmalts <- function(list){
  MaltDirectory$group <- 2
  group<-MaltDirectory$group
  for(malt in(list)){
    i <-grep(malt, MaltDirectory$Malt,ignore.case=TRUE)
    group[i]<-1
  }
  return(group)
}








###End creating searchable malt directory
