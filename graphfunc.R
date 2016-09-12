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
  
  maltgraph<- ggplot(subset(MaltDirectory, MaltDirectory$G>1020), aes(x=G, y= L, colour=as.character(group)))+
    geom_jitter()+
    scale_colour_manual(values=c("#00DCF2","#00F28F"))+
    scale_size_manual(values=c(2,1))+
    geom_text(aes(label=ifelse(group==1,as.character(Malt),'')),hjust=0, colour="#8f00f2")+
    theme(legend.position="none")+
    plot_theme()+
    xlim(min(subset(MaltDirectory, group == 1)$G)-5,max(subset(MaltDirectory, group == 1)$G)+5)+
    ylim(min(subset(MaltDirectory, group == 1)$L)-25,max(subset(MaltDirectory, group == 1)$L)+25)
  
  
  return(maltgraph)
}

crattgr <- function(gravityNotesDT){
  clr <- checkSrm(gravityNotesDT[4,])
  Attgraph <- ggplot(gravityNotesDT[c(2,3,4,6),], aes(x=gravityNotesDT$Info,y=Value, fill="skip"))+
    geom_point(aes(size = 30, colour = clr), gravityNotesDT[c(2,3,4,6),])+
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


stjerne <- data.frame(read.csv(file = "XY.csv"))
stjerne$Group <- factor(6)
south <- data.frame(read.csv(file = "XYsouth.csv"))
south$Group <- factor(7)
hopimg <- data.frame(read.csv(file = "XYhop.csv"))
hopimg$Group <- factor(7)
leaves <- data.frame(read.csv(file = "XYleaves.csv"))
leaves$Group <- as.factor(leaves$Group)
stem <- data.frame(read.csv(file = "XYstem.csv"))
stem$Group <- factor(10)
lineimg <- data.frame(read.csv(file = "XYline.csv"))
lineimg$Group <- factor(11)
brew <- data.frame(read.csv(file = "XYbrew.csv"))
brew$Group <- factor(7)
star <- data.frame(read.csv(file = "XYstar.csv"))
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



logo <- ggplot(data = south, aes(Xsouth,Ysouth, color = Group))+
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
  ))+
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
