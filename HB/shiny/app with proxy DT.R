##install.packages("shinydashboard")
##install.packages("shinytheme")
###install.packages("directlabels")
###install.packages("extrafont")
##install.packages("useful")
##font_import() # Import all fonts
##fonts() # Print list of all fonts
##install.packages("jpeg")
library(shiny) 
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
#library(ggplot2)
#library(stringr)
#library(plyr)
#library(reshape2)
#library(directlabels)
#library(grid)
#library(extrafont)
#library(useful)
#library(jpeg)
#library(plotly)
#library(data.table)


setwd("S:/Peder/training/R/HB")

source(paste0(getwd(),"/maltsdirectory.R"))
source(paste0(getwd(),"/hopsdirectory.R"))
source(paste0(getwd(),"/yeastdirectory.R"))
source(paste0(getwd(),"/boilmashcalc.R"))
source(paste0(getwd(),"/gravitycolourcalc.R"))
source(paste0(getwd(),"/ibucalc.R"))
source(paste0(getwd(),"/graphfunc.R"))

getMaltsShiny <- function(Malts){
  for(malt in 1:length(Malts)){
    if(malt == 1){
      Malt <- searchmalts(Malts[malt])
    } else {
      Malt <- rbind(Malt, searchmalts(Malts[malt]))
    }
  }
  return(Malt)
}

gethopsShiny <- function(Hops){
  for(hop in 1:length(Hops)){
    if(hop == 1){
      Hop <- searchhops(Hops[hop])
    } else {
      Hop <- rbind(Hop, searchhops(Hops[hop]))
    }
  }
  return(Hop)
}

getyeastsShiny <- function(Yeasts){
  for(yeast in 1:length(Yeasts)){
    if(yeast == 1){
      Yeast <- searchYeastsSimple(Yeasts[yeast])
    } else {
      Yeast <- rbind(Yeast, searchYeastsSimple(Yeasts[yeast]))
    }
  }
  return(Yeast)
}


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


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
  ),
  
  dashboardSidebar(sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                     label = "Search..."),
                   sidebarMenu(
                     textInput("BName",label = h4("Brew name:"), value = ""),
                     textInput("BType",label = h4("Brew type:"), value = ""),
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Ingredients", tabName = "Ingredients", icon = icon("th"),
                              badgeLabel = "new", badgeColor = "green"),
                     menuItem("Information", tabName = "Information", icon = icon("th"),
                              badgeLabel = "new", badgeColor = "green"),
                     actionButton("compute", "Calculate recipe", width = "100%", icon("paper-plane"), 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     menuItem("Malts", icon = icon("grain", lib = "glyphicon"),
                              href = "https://www.beeradvocate.com/beer/101/malts/",
                              badgeLabel = "opens new page", badgeColor = "blue"),
                     menuItem("Hops", icon = icon("certificate", lib = "glyphicon"),
                              href = "https://www.beeradvocate.com/beer/101/hops//",
                              badgeLabel = "opens new page", badgeColor = "blue"),
                     menuItem("Yeasts", icon = icon("flask"),
                              href = "https://www.beeradvocate.com/beer/101/yeast/",
                              badgeLabel = "opens new page", badgeColor = "blue")
                     
                   )
                   
  ),
  
  dashboardBody(
    tabItems(id = "Body",
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tabBox(
                  title = "Enter ingredients", height = "640px",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  tabPanel("Tab1",
                           em("Enter values separated by a comma. You must enter all values at once, ie 'Munich, chocolate, pilsner'"),
                           textInput("Malts",label = h3("Malts:"), value = ""),
                           actionButton("do", "Show Choices"),
                           textInput("Hops",label = h3("Hops:"), value = ""),
                           actionButton("do2", "Show Choices"),
                           textInput("Yeasts",label = h3("Yeasts:"), value = ""),
                           actionButton("do3", "Show Choices")),
                  tabPanel("Tab2",
                           plotOutput("batch5")
                  ),
                  tabPanel("Tab3",
                           plotOutput("batch6"))
                ),
                
                box(
                  title = "Controls", height = "640px",
                  sliderInput("slider", "Target batch (L):", 0,50, 20),
                  sliderInput("slider2", "Mash time (Min):", 0,120, 60),
                  sliderInput("slider3", "Boil time (Min):", 0,120, 60),
                  sliderInput("slider4", "Target mash temp (Cel):", 0,100, 64),
                  sliderInput("slider5", "Grain temp (Cel):", 0,50, 22),
                  sliderInput("slider5", "Brewhouse efficiency:", 0,1, .72)
                  
                  #Grams_Grain <- 7348
                )
              ),
              tabsetPanel(title = "Options", id = "tabset2",
                          tabPanel("Malts",
                                   conditionalPanel(
                                     condition = "searchmalt",
                                     DT::dataTableOutput('Maltoptions')
                                   )),
                          tabPanel("Hops",
                                   
                                   conditionalPanel(
                                     condition = "searchhop",
                                     DT::dataTableOutput('Hopoptions')
                                   )),
                          tabPanel("Yeasts",
                                   
                                   conditionalPanel(
                                     condition = "searchyeast",
                                     DT::dataTableOutput('Yeastoptions')
                                   ))
              )
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "Ingredients",
              h2("Ingredients"),
              fluidRow(
                tabsetPanel(
                  title = "IngredientsTab",
                  id = "IngredientsTab",
                  tabPanel("Maltstable",
                           actionButton("tbl1", "Load table"),
                           DT::dataTableOutput("maltreduced")
                  ),
                  tabPanel("Hopstable",
                           actionButton("tbl2", "Load table"),
                           DT::dataTableOutput("hopreduced")
                  ),
                  tabPanel("Yeasttable",
#                           actionButton("tbl3", "Load table"),
                           DT::dataTableOutput("yeastreduced")
                  )
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "Information",
              h2("Information"),
              fluidRow(
                tabBox(
                  title = "InformationTab",
                  id = "Information", height = "400px", width = 12,
                  tabPanel("Batch",
                           DT::dataTableOutput("batch")
                           
                  ),
                  tabPanel("Colour and gravity",
                           DT::dataTableOutput("batch2")
                  ),
                  tabPanel("Hops and bitterness",
                           DT::dataTableOutput("batch3"),
                           plotOutput("batch4")
                  )
                  #items
                )
              )
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
    #https://groups.google.com/forum/#!msg/shiny-discuss/FeqU0AoTpz0/Zooap7iCIu8J  
    # create a character vector of shiny inputs
    shinyInput = function(FUN, len, id, ...) {
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }
    
    # obtain the values of inputs
    shinyValue = function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value)) NA else value
      }))
    } 
    
    observeEvent(input$compute, {
      updateTabItems(session, "Body", selected = "Information")
    })


    ###########################
    ########Maltbutton#########
    ###########################
    
    
  searchmalt <- eventReactive(input$do, data.frame(
    rbind(
      getMaltsShiny({gsub(" ","",unlist(strsplit(input$Malts, ",")))}),
      shinyInput(textInput, ncol(getMaltsShiny({gsub(" ","",unlist(strsplit(input$Malts, ",")))})), 'v1_', value = "Add malt", width = "100px")
      )
  )
    )
  
  # render the table containing shiny inputs
  output$Maltoptions = DT::renderDataTable(
    searchmalt(), server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      )
  )
  

  # print the values of inputs
  output$x2 = renderPrint({
    input$Maltoptions_rows_selected
  })
  
 
  
  observeEvent(input$do, {
    updateTabsetPanel(session, "tabset2", selected = "Malts")
  })
  

  selections <- eventReactive(input$tbl1, 
    if(nrow(searchmalt()) %in% input$Maltoptions_rows_selected){
      input$Maltoptions_rows_selected[-length(input$Maltoptions_rows_selected)]
    }else {
      input$Maltoptions_rows_selected
    }
  )
  
  extraRow <- eventReactive(input$tbl1,
    if(nrow(searchmalt()) %in% input$Maltoptions_rows_selected){
      shinyValue('v1_', ncol(searchmalt()))
    }else{
      NULL
    })

  Maltoptionsred <- eventReactive(input$tbl1, data.frame(
    rbind(
      searchmalt()[selections(),],
      extraRow()))
    )
  

  
  newMaltoptions <- eventReactive(input$tbl1, data.frame(
    cbind(Maltoptionsred(),
          shinyInput(numericInput, nrow(Maltoptionsred()), 'v2_', value = "", width = "100px"))
  ))
  
  
  output$maltreduced <- DT::renderDataTable(
    newMaltoptions(), server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      )
    ,
    colnames = c(colnames(Maltoptionsred()), "Amount (g)")
    
  )
  
  ###########################
  ########Hopbutton#########
  ###########################
  
  
  presearchhop <- eventReactive(input$do2, data.frame(
          rbind(
      gethopsShiny({gsub(" ","",unlist(strsplit(input$Hops, ",")))}),
      shinyInput(textInput, ncol(gethopsShiny({gsub(" ","",unlist(strsplit(input$Hops, ",")))})), 'Hopv1_', value = "Add hop", width = "100px")
    )[c(1,6,7,24,25,26,33,37,38,39)]
  )
  )
  
  searchhop <- eventReactive(input$do2, data.frame(
    cbind(
      shinyInput(textInput, nrow(presearchhop()), 'Hopadd_', value = "", width = "40px"),
      presearchhop()
  )
  ))
  
  # render the table containing shiny inputs
  output$Hopoptions = DT::renderDataTable(
    searchhop(), server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      ),
    colnames = c("Num.Of.Additions",colnames(presearchhop()))
      )
  

  # print the values of inputs
  output$x2hop = renderPrint({
    shinyValue('Hopv2_', sum(as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected])))
  })
  
  observeEvent(input$do2, {
    updateTabsetPanel(session, "tabset2", selected = "Hops")
  })
  
  
  hopselections <- eventReactive(input$tbl2, 
                                 if(nrow(searchhop()) %in% input$Hopoptions_rows_selected){
                                   input$Hopoptions_rows_selected[-length(input$Hopoptions_rows_selected)]
                                 }else {
                                   input$Hopoptions_rows_selected
                                 }
  )
  
  HopextraRow <- eventReactive(input$tbl2,
                               if(nrow(searchhop()) %in% input$Hopoptions_rows_selected){
                                 shinyValue('Hopv1_', ncol(searchhop()))
                               }else{
                                 NULL
                               })
  
  Hopoptionsred <- eventReactive(input$tbl2, data.frame(
    rbind(
      searchhop()[hopselections(),],
      HopextraRow())
    )
    
  )
  

  #Start here
  Hopoptionsexp <- eventReactive(input$tbl2, data.frame(
    Hopoptionsred()[-1][rep(1:nrow(Hopoptionsred()), as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected])),]
  ))
  

  newHopoptions <- eventReactive(input$tbl2, reactiveValues(data = data.frame(
    cbind(
      #https://github.com/rstudio/DT/issues/178
      shinyInput(numericInput, nrow(Hopoptionsexp()), 'Hopv2_', value = 0, width = "100px"),
      shinyInput(numericInput, nrow(Hopoptionsexp()), 'Time_', value = 60, width = "100px"),
      Hopoptionsexp()
    )
  )))
  

  output$hopreduced <- DT::renderDataTable(
    newHopoptions()$data[order(newHopoptions()$data$Name),], server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      ),
    colnames = c("Amount (g)","Boil time (min)",colnames(Hopoptionsred()[-1]))
    
      )
  

#  testnumbers <- c("9 2" , "39 1")
#  lapply(testnumbers, function(x) unlist(strsplit(x," "))[[2]])
#  testrows <- HopDirectory[1:3,]
#  for(rownumber in list(1,3)){
#    for(manytime in lapply(testnumbers, function(x) unlist(strsplit(x," "))[[2]])){
#      testrows <- rbind(testrows,testrows[rownumber,])
#    }
#    return(testrows[order(testrows$Name),])
#  }
  
  ###########################
  ########Yeastbutton#########
  ###########################
  
  searchyeast <- eventReactive(input$do3, data.frame(
    rbind(
      getyeastsShiny({gsub(" ","",unlist(strsplit(input$Yeasts, ",")))}),
      shinyInput(textInput, ncol(getyeastsShiny({gsub(" ","",unlist(strsplit(input$Yeasts, ",")))})), 'Yeastv1_', value = "Add yeast", width = "100px")
    )[-1],
    stringsAsFactors = FALSE
  )
  )
  
  # render the table containing shiny inputs
  output$Yeastoptions = DT::renderDataTable(
    searchyeast(), server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      )
      )
  # print the values of inputs
  output$x2yeast = renderPrint({
    list(shinyValue('Yeastv2_', ncol(searchyeast())), YeastextraRow())
  })
  
  observeEvent(input$do3, {
    updateTabsetPanel(session, "tabset2", selected = "Yeasts")
  })
  
  yeastselections <- eventReactive(input$Yeastoptions_rows_selected, 
                                   if(nrow(searchyeast()) %in% input$Yeastoptions_rows_selected){
                                     input$Yeastoptions_rows_selected[-length(input$Yeastoptions_rows_selected)]
                                   }else {
                                     input$Yeastoptions_rows_selected
                                   }
  )
  
  YeastextraRow <- eventReactive(input$Yeastoptions_rows_selected,
                                 if(nrow(searchyeast()) %in% input$Yeastoptions_rows_selected){
                                   shinyValue('Yeastv1_', ncol(searchyeast()))
                                 }else{
                                   NULL
                                 })
  
  Yeastoptionsred <- eventReactive(input$Yeastoptions_rows_selected, data.frame(
    rbind(
      searchyeast()[yeastselections(),],
      YeastextraRow()))
  )
  
  newYeastoptions <- eventReactive(input$Yeastoptions_rows_selected, data.frame(
    cbind(Yeastoptionsred(),
          shinyInput(numericInput, nrow(Yeastoptionsred()), 'Yeastv2_', value = "", width = "100px"))
  ))
  
  
  output$yeastreduced <- DT::renderDataTable(
    newYeastoptions(), server = FALSE, escape = FALSE, options = list(
      preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
      ),
    colnames = c(colnames(Yeastoptionsred()), "Amount (g)")
    
      )
  

  
  #information
  #grain
  
  #Next step: finding grams_grain input
  output$grams_grain <- eventReactive(input$compute, ShinyIBUCalc(Hops = newHopoptions()$data[order(newHopoptions()$data$Name),], Hopweights = shinyValue('Hopv2_', sum(as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected]))), Target_Batch_L = input$slider, OGBE = gravityNotes()[[1]][1], Boilmin =  shinyValue('Time_', sum(as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected]))))
                                      
#ShinyIBUCalc(Hops = Hopoptionsred()[-1], Hopweights = shinyValue('Hopv2_', nrow(Hopoptionsred())), #Target_Batch_L = input$slider, OGBE = gravityNotes()[[1]][1], Boilmin =  shinyValue('Time_', nrow#(Hopoptionsred())))
    
    #Hops, Hopweights, Target_Batch_L, OGBE, Boilmin
  )
  
  gravityNotes <- eventReactive(input$compute, ShinyFGCalc(newMaltoptions(), shinyValue('v2_', nrow(newMaltoptions())), input$slider, newYeastoptions(), input$slider5))
  
  recipeNotes <-  eventReactive(input$compute, amountfunc(Target_Batch_L = input$slider, Grams_Grain = sum(shinyValue('v2_', nrow(newMaltoptions()))), Boil_time_Hr = input$slider3, Target_Mash_Temp = input$slider4, Grain_Temp= input$slider5))
  
  hopnotes <-  eventReactive(input$compute, ShinyIBUCalc(Hops = newHopoptions()$data[order(newHopoptions()$data$Name),], Hopweights = shinyValue('Hopv2_', sum(as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected]))), Target_Batch_L = input$slider, OGBE = gravityNotes()[[1]][1], Boilmin =  shinyValue('Time_', sum(as.numeric(shinyValue('Hopadd_', nrow(searchhop()))[input$Hopoptions_rows_selected])))))
  
  recipeNotesDT <- eventReactive(input$compute, data.frame(
    Info = c("Strike Volume","Sparge Volume","Total Water", "Strike temperature","Target batch (L)","Target Mash temperature"),
    Value = recipeNotes(),
    stringsAsFactors = FALSE 
  ))
  
  gravityNotesDT <- eventReactive(input$compute, data.frame(
    Info = c("OGBE","OGBEgal","SRM","MCU","EFG","EABV"),
    Value = gravityNotes()[[1]],
    stringsAsFactors = FALSE
  ))
  
  hopnotesDT <- eventReactive(input$compute, data.frame(
    Info = c("IBUs"),
    Value = round(sum(as.numeric(hopnotes()$IBU))),
    stringsAsFactors = FALSE
  ))
  
   output$batch <- DT::renderDataTable(
    recipeNotesDT(), server = FALSE, escape = FALSE
  )
   
   output$batch2 <- DT::renderDataTable(
     gravityNotesDT(), server = FALSE, escape = FALSE
   )
   
   output$batch3 <- DT::renderDataTable(
     hopnotesDT(), server = FALSE, escape = FALSE
   )
  
   output$batch4 <- renderPlot(
     crhopgr(newHopoptions()$data[order(newHopoptions()$data$Name),])
   )
   
   output$batch5 <- renderPlot(
     crmaltgr(newMaltoptions()$Malt)
   )
   
   output$batch6 <- renderPlot(
     crattgr(gravityNotesDT())
   )


  #"slider", "Target batch (L):", 0
  #"slider2", "Mash time (Min):", 0
  #"slider3", "Boil time (Min):", 0
  #"slider4", "Target mash temp (Ce
  #"slider5", "Grain temp (Cel):", 
  #"slider5", "Brewhouse efficiency
  

  
}

#add source boilmash
#add kg etc. converters
#add button
#add information tab

#Add recipe storage


shinyApp(ui, server)






