
srmHex <- data.frame(read.csv(file = "U:/R/HB/SRMHEX.csv"))

checkSrm <- function(SRM){
  SRM <- ifelse(round(SRM) > 40, 40, round(SRM))
  for(col in 1:nrow(srmHex)){
    if(srmHex[col,1] == SRM){
      HEX <- as.character(srmHex[col,2])
      colr <- paste0("#",HEX)
    }
  }
  return(colr)
}

checkSrm(34)

#http://www.color-hex.com/color/fae8aa
plot_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    panel.grid = element_line(colour = "#f0f0f0"),
    axis.ticks.margin = "none",
    axis.line=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none"
  )
}
###Start Overall function
if(file.exists('U:\\Documents\\misc\\Logo.jpg') == TRUE){
  img<-'U:\\Documents\\misc\\Logo.jpg'
}

#img<-rasterGrob(img, interpolate=TRUE)


RunFGCalc <- function(Malts, Grams_Grain, Grainprops, Target_Batch_L, Hops, Hopweights, Boilmin, Boil_time_Hr, Target_Mash_Temp, yeast, Grain_Temp=NULL, brewhouse_efficiency=NULL, BeerName=NULL, BeerType=NULL){

  gravity_notes_step<-(FGCalc(Malts,Grams_Grain, Grainprops,Target_Batch_L, yeast, brewhouse_efficiency=NULL))
  yeasts <- gravity_notes_step[[3]]
  gravity_notes <- gravity_notes_step[[1]]
  Malts <- gravity_notes_step[[2]]
  OGBE<-as.numeric(gravity_notes[1])
  recipeNotes <- amountfunc(Target_Batch_L, Grams_Grain, Boil_time_Hr, Target_Mash_Temp, Grain_Temp)
  IBU<-IBUCalc(Hops, Hopweights, Target_Batch_L, OGBE, Boilmin)
#  Malts <- gravity_notes[7]
  NAMES<-c("OG","FG","OG/gal","SRM","MCU","ABV","IBU")
  Beer_info<-c(gravity_notes,IBU)
  Beer_info<-c(Beer_info[1],Beer_info[5],Beer_info[2],Beer_info[3],Beer_info[4],Beer_info[6],Beer_info[7])
  Beer_info[1]<-as.numeric(Beer_info[1])
  Beer_info[2]<-as.numeric(Beer_info[2])
  Beer_info[6]<-as.numeric(Beer_info[6])
  df <- melt(data.frame(NAMES, Beer_info))
  colnames(df)<- c("info","skip","result")
  df$info2<- factor(df$info, as.character(df$info))
  if(is.null(BeerName)){
    BeerName<-readline(prompt = "Enter Beer Name: ")
  }
  if(is.null(BeerType)){
    BeerType<-readline(prompt = "Enter Beer Type: ")
  }
  clr <- checkSrm(Beer_info[4])
  HopDirectory$group<-grouphops(Hops)
  hopgraph<-ggplot(HopDirectory, aes(x=reorder(Name,-Alpha.Acid.High),(0-cohumulone), fill=as.factor(group) ))+
    theme(axis.text.x = element_blank(),
          legend.position="none")+
    #colour=c("#00B259","#A9A9A9")[as.factor(HopDirectory$group)]
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acid.High)), stat="identity",alpha=.5)+
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-Alpha.Acids)), stat="identity", alpha=.35)+
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),(0-cohumulone)), stat="identity")+
    #acids per 1000g
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Farneseneplot), stat="identity",alpha=.25)+
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Caryophylleneplot), stat="identity",alpha=.5)+
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*Humuleneplot), stat="identity",alpha=.75)+
    geom_bar(aes(x=reorder(Name,-Alpha.Acid.High),10*MyrcenePER100G), stat="identity")+
    geom_abline(aes(slope=0, intercept=0, colour = "white", alpha = 0.2))+
    scale_x_discrete(name="Hop Name")+
    scale_y_continuous(name="Mls/100 g dried hops//Alpha Acids")+
    scale_fill_manual(values=c("#00DCF2","#00F28F"))+
    geom_text(aes(label=ifelse(group==1,as.character(Name),'')),hjust=0, angle = 90, size=4, colour="#8f00f2")+
    geom_text(data=HopDirectory[4,], aes(x=reorder(Name,-Alpha.Acid.High), (0-cohumulone)),label="Cohumulone",hjust=0,vjust=-1, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[4,], aes(x=reorder(Name,-Alpha.Acid.High), (0-Alpha.Acids)),label="Alpha acids",hjust=0,vjust=-3, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[4,], aes(x=reorder(Name,-Alpha.Acid.High), (0-Alpha.Acid.High)),label="Alpha acid range",hjust=0,vjust=-1, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[39,], aes(x=reorder(Name,-Alpha.Acid.High), 10*Farneseneplot),label="Farnesene",hjust=0,vjust=0, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[18,], aes(x=reorder(Name,-Alpha.Acid.High), 10*Caryophylleneplot),label="Caryophyllene",hjust=0,vjust=1, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[4,], aes(x=reorder(Name,-Alpha.Acid.High), 10*Humuleneplot),label="Humulene",hjust=0,vjust=2, size=3, colour="#8f00f2")+
    geom_text(data=HopDirectory[4,], aes(x=reorder(Name,-Alpha.Acid.High), 10*MyrcenePER100G),label="Myrcene",hjust=0,vjust=3, size=3, colour="#8f00f2")+
    plot_theme()
  MaltDirectory$group<-groupmalts(Malts)
  maltgraph<- ggplot(subset(MaltDirectory, MaltDirectory$G>1020), aes(x=G, y= L, colour=as.character(group), size=as.character(group)))+
    geom_jitter()+
    scale_colour_manual(values=c("#00DCF2","#00F28F"))+
    scale_size_manual(values=c(2,1))+
    geom_text(aes(label=ifelse(group==1,as.character(Malt),'')),hjust=0, size = 3, colour="#8f00f2")+
    theme(legend.position="none")+
    plot_theme()
  Attgraph <- ggplot(df[c(3:7),], aes(y=result, fill="skip"))+
    geom_point(aes(x=info2, size = 30, colour = clr), data=df[c(3:7),])+
#    coord_polar()+
    scale_color_manual(values=clr)+
    theme(
      plot.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
      panel.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
      panel.grid = element_line(colour = "#f0f0f0"),
      panel.border = element_rect(fill = NA, colour = "#00f28f", size = 2),
      axis.ticks.margin = "none",
      axis.line=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none"
    )+
    geom_text(aes(4,(df[7,]$result+10), label = "Colour = SRM", size = 30, colour = clr))
  Attgraph
  returnlist <- list(gravity_notes, IBU, df, BeerName, BeerType, hopgraph, maltgraph, Attgraph, img, Hops, Malts, yeasts, Grainprops, recipeNotes, Hopweights)
  filename <- paste0("U:/R/HB/", BeerName, gsub(" ","", format(Sys.time(), "%d %m %y %H %M")),".Rdata")
  save(returnlist, file = filename)
  return(filename)
}
