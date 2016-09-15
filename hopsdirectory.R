######################################
#### Creating NEW Searchable Hop index
#####################################
#setwd('S:\\Peder\\training\\Python')
getwd()
mydata <- data.frame(read.csv("Hops directory.csv",sep=",",header=TRUE, stringsAsFactors = FALSE))

mydata$Name <- ifelse(mydata$Name == 'DORADO','ELDORADO',as.character(mydata$Name))
mydata[14,1]<-'COLUMBUS TOMAHAWK ZEUS'
mydata[41,1]<- 'SUPER GALENA'
mydata$approxalpha= (as.numeric(mydata$Alpha.Acids)+as.numeric(mydata$Alpha.Acid.High))/2
mydata$approxbeta= (as.numeric(mydata$Beta.Acids)+as.numeric(mydata$Beta.Acid.High))/2
mydata$cohumulone= (((as.numeric(mydata$Cohumulone...of.alpha.acids)+as.numeric(mydata$Cohumulone....high.))/2)/100)*mydata$approxalpha

mydata$totaloils100g <- (mydata$Total.Oils.Mls.per.100.grams.dried.hops  + mydata$Total.Oils.High..Mls.)/2


mydata$Farnesene.as...of.other.oils<-gsub("0.1.0","1.0",mydata$Farnesene.as...of.other.oils)
mydata$FarnesenePER100G<-(as.numeric(gsub("[ < ]", "", mydata$Farnesene.as...of.other.oils))/100)*mydata$totaloils100g 


mydata$Farnesene<-as.numeric(gsub("[ < ]", "", mydata$Farnesene.as...of.other.oils))
mydata$Myrcene= ((as.numeric(mydata$Myrcene.as...of.other.oils  )+as.numeric(mydata$Myrcene.high))/2)
mydata$Caryophyllen= ((as.numeric(mydata$Caryophyllene.as...of.other.oils  )+as.numeric(mydata$Caryophyllene.high  ))/2) 
mydata$Humulene= ((as.numeric(mydata$Humulene.as...of.other.oils)+as.numeric(mydata$Humulene.high))/2)


mydata$MyrcenePER100G= (((as.numeric(mydata$Myrcene.as...of.other.oils  )+as.numeric(mydata$Myrcene.high))/2)/100)*mydata$totaloils100g 
mydata$CaryophyllenPER100G= (((as.numeric(mydata$Caryophyllene.as...of.other.oils  )+as.numeric(mydata$Caryophyllene.high  ))/2)/100)*mydata$totaloils100g 
mydata$HumulenePER100G= (((as.numeric(mydata$Humulene.as...of.other.oils)+as.numeric(mydata$Humulene.high))/2)/100)*mydata$totaloils100g 




mydata2<-list(mydata$Name,
              as.numeric(mydata$MyrcenePER100G),
              as.numeric(mydata$CaryophyllenPER100G),
              as.numeric(mydata$HumulenePER100G),
              as.numeric(mydata$FarnesenePER100G)
)


mydata$alpharange<-mydata$Alpha.Acid.High-mydata$Alpha.Acids
mydata3<-list(mydata$Name,
              as.numeric(mydata$Alpha.Acids),
              as.numeric(mydata$alpharange),
              as.numeric(mydata$cohumulone),
              as.numeric(mydata$Beta.Acids)
)

mydata2<-as.data.frame(mydata2)
list1<-names(mydata2)
list<-c("Name","Myrcene","Caryophyllen","Humulene","Farnesene")
setnames(mydata2, old = list1, new = list)

mydata3<-as.data.frame(mydata3)
list1<-names(mydata3)
list<-c("Name","Alpha.Acids","Alpha.Acid.High","cohumulone","Beta.Acids")
setnames(mydata3, old = list1, new = list)

mydata2 <- melt(mydata2, id.var="Name")
mydata3 <- melt(mydata3, id.var="Name")
list1<-names(mydata3)
list<-c("Name","variable2","value2")
setnames(mydata3, old = list1, new = list)

mydata2 <- cbind(mydata2,mydata3[2:3])

sapply(mydata$Alpha.Acid.High,class)


mydata$Humuleneplot <- mydata$MyrcenePER100G+mydata$HumulenePER100G
mydata$Caryophylleneplot <- mydata$MyrcenePER100G+mydata$HumulenePER100G+mydata$CaryophyllenPER100G
mydata$Farneseneplot <- mydata$MyrcenePER100G+mydata$HumulenePER100G+mydata$CaryophyllenPER100G+mydata$FarnesenePER100G

HopDirectory <- mydata

HopDirectory$sugguse <- ifelse(HopDirectory$approxalpha <=7, "base hop", NA)
HopDirectory$group <- 2


addRowH <- function(list){
  HopDirectory <- bind_rows(HopDirectory, as.data.frame(list))
  return(HopDirectory)
}






p<-ggplot(HopDirectory, aes(x=reorder(Name,-Alpha.Acid.High) ))
p+geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acid.High)), stat="identity", fill='#DEB887', alpha=.5)+
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acids)), stat="identity", fill='#DEB887', alpha=.35)+
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-cohumulone)), stat="identity", fill='#DEB887')+
  #geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Beta.Acids)), stat="identity", fill='#8B4513')+
  #acids per 1000g
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Farneseneplot), stat="identity", fill='#3CB371',alpha=.25)+
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Caryophylleneplot), stat="identity", fill='#3CB371', alpha=.5)+
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Humuleneplot), stat="identity", fill='#3CB371', alpha=.75)+
  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*MyrcenePER100G), stat="identity", fill='#3CB371')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(name="Hop Name")+
  scale_y_continuous(name="Mls/100 g dried hops//Alpha Acids")

#theme(panel.background = element_rect(fill = 'green', colour = 'red'))


#HopDirectory$approxalpha<-as.numeric(HopDirectory$approxalpha)

searchhops <- function(name){
  i <-grep(name, HopDirectory$Name,ignore.case=TRUE)
  return(HopDirectory[i,])
}

##Creating function to group inside list vs outside list hops for graph

grouphops <- function(list){
  HopDirectory$group <- 2
  group<-HopDirectory$group
  for(hop in(list)){
    i <-grep(hop, HopDirectory$Name,ignore.case=TRUE)
    group[i]<-1
  }
  return(group)
}



#Hops<-c("Cascade", "Hallertau","Amarillo","Warrior")

#HopDirectory$group<-grouphops(Hops)
#searchhops("Warrior")



#HopDirectory$group<-grouphops(Hops)
#p<-ggplot(HopDirectory, aes(x=reorder(Name,-Alpha.Acid.High), fill=as.factor(group) ))+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acid.High)), stat="identity",alpha=.5)+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acids)), stat="identity", alpha=.35)+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-cohumulone)), stat="identity")+
#  #acids per 1000g
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Farneseneplot), stat="identity",alpha=.25)+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Caryophylleneplot), stat="identity",alpha=.5)+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Humuleneplot), stat="identity",alpha=.75)+
#  geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*MyrcenePER100G), stat="identity")+
#  geom_hline(aes(yintercept=0, colour="#A9A9A9"))+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  scale_x_discrete(name="Hop Name")+
#  scale_y_continuous(name="Mls/100 g dried hops//Alpha Acids")+
#  scale_fill_manual(values=c("#00B259","#A9A9A9"))

#summary(HopDirectory$approxalpha)
#lowaa<-subset(HopDirectory, approxalpha >7 & approxalpha<10)







#### End creating Searchable Hop index
