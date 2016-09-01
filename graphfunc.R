plot_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
    panel.grid = element_line(colour = "#f0f0f0"),
    axis.ticks.margin = "none",
    axis.line=element_blank(),
    #axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    legend.position="none"
  )
}
srmHex <- data.frame(read.csv(file = "SRMHEX.csv"))

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



crhopgr <- function(Hops){
  
  HopDirectory$group <- grouphops(Hops$Name)
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
  
  return(hopgraph)
  
}
 
crmaltgr <- function(Malts) {
  MaltDirectory$group<-groupmalts(Malts)
  
  maltgraph<- ggplot(subset(MaltDirectory, MaltDirectory$G>1020), aes(x=G, y= L, colour=as.character(group), size=as.character(group)))+
    geom_jitter()+
    scale_colour_manual(values=c("#00DCF2","#00F28F"))+
    scale_size_manual(values=c(2,1))+
    geom_text(aes(label=ifelse(group==1,as.character(Malt),'')),hjust=0, colour="#8f00f2")+
    theme(legend.position="none")+
    plot_theme()
  
  return(maltgraph)
}

crattgr <- function(gravityNotesDT){
  clr <- checkSrm(gravityNotesDT[4,])
  Attgraph <- ggplot(gravityNotesDT[c(2,3,4,6),], aes(y=Value, fill="skip"))+
    geom_point(aes(x=gravityNotesDT$Info, size = 30, colour = clr), gravityNotesDT[c(2,3,4,6),])+
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
    geom_text(aes(4,(gravityNotesDT[6,]$value+10), label = "Colour = SRM", size = 30, colour = clr))
  return(Attgraph)
}
