##install.packages("shinydashboard")
##install.packages("shinytheme")
#library(shiny)
#library(shinydashboard)
#library(shinytheme)
#library(ggplot2)
#library(stringr)
#library(plyr)
#library(dplyr)
#library(tidyr)
#library(data.table)
#library(reshape2)
#library(shiny)
###install.packages("directlabels")
#library(directlabels)
#library(grid)
###install.packages("extrafont")
#library(extrafont)
##install.packages("useful")
#library(useful)
##font_import() # Import all fonts
##fonts() # Print list of all fonts
##install.packages("jpeg")
#library(jpeg)
#library(plotly)
library(DT)


setwd("S:/Peder/training/R/HB")

source(paste0(getwd(),"/maltsdirectory.R"))
source(paste0(getwd(),"/hopsdirectory.R"))
source(paste0(getwd(),"/yeastdirectory.R"))
source(paste0(getwd(),"/boilmashcalc.R"))

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
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Ingredients", tabName = "Ingredients", icon = icon("th"),
                              badgeLabel = "new", badgeColor = "green"),
                     menuItem("Information", tabName = "Information", icon = icon("th"),
                              badgeLabel = "new", badgeColor = "green"),
                     menuItem("Malts", icon = icon("grain", lib = "glyphicon"),
                              href = "https://www.beeradvocate.com/beer/101/malts/",
                              badgeLabel = "opens new page", badgeColor = "blue"),
                     menuItem("Hops", icon = icon("certificate", lib = "glyphicon"),
                              href = "https://www.beeradvocate.com/beer/101/hops//",
                              badgeLabel = "opens new page", badgeColor = "blue"),
                     menuItem("Yeasts", icon = icon("flask"),
                              href = "https://www.beeradvocate.com/beer/101/yeast/",
                              badgeLabel = "opens new page", badgeColor = "blue"),
                     actionButton("compute", "Calculate recipe", width = "100%", icon("paper-plane"), 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   )
                   
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tabBox(
                  title = "Enter ingredients", height = "640px",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  tabPanel("Tab1",textInput("Malts",label = h3("Malts:"), value = ""),
                           actionButton("do", "Show Choices"),
                           textInput("Hops",label = h3("Hops:"), value = ""),
                           actionButton("do2", "Show Choices"),
                           textInput("Yeasts",label = h3("Yeasts:"), value = ""),
                           actionButton("do3", "Show Choices")),
                           textInput("Gramsgrain",label = h3("Grain (grams):"), value = ""),
                  tabPanel("Tab2",plotOutput("plot1", height = 350))
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
                      condition = "Malttext",
                      uiOutput("Malttext")
                    )),
                tabPanel("Hops",
                         
                         conditionalPanel(
                           condition = "Hoptext",
                           uiOutput("Hoptext")
                    )),
                tabPanel("Yeasts",
                         
                         conditionalPanel(
                           condition = "Yeasttext",
                           uiOutput("Yeasttext")
                    ))
              )
  
              
      ),
      
      # Second tab content
      tabItem(tabName = "Ingredients",
              h2("Ingredients"),
              fluidRow(
                tabBox(
                  title = "IngredientsTab",
                  id = "IngredientsTab", height = "400px", width = 12,
                  tabPanel("Maltstable",
                           actionButton('newmalt', 'Add Malt'),
                            dataTableOutput("searchmalttable")
                             ),
                  tabPanel("Hopstable",
                           dataTableOutput("searchhoptable")
                  ),
                  tabPanel("Yeasttable",
                           dataTableOutput("searchyeasttable")
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
                           textOutput("batch")
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
  #input$searchText
  #input$searchButton
  
  #  output$Malts<- renderPrint({unlist(strsplit(input$Malts, ","))})
  #  output$Hops <- renderPrint({length(unlist(strsplit(input$Hops, ",")))})
  #  output$Yeasts <- renderPrint({input$Yeasts})
  
  #Maltbutton
  searchmalt <- eventReactive(input$do, getMaltsShiny({gsub(" ","",unlist(strsplit(input$Malts, ",")))}))
  
  observeEvent(input$do, {
    updateTabsetPanel(session, "tabset2", selected = "Malts")
  })
  
  output$Malttext = renderUI({
    Malts <- searchmalt()[,1]
    checkboxGroupInput("Maltscbox","Choose Malts", Malts)
  })

  searchmalttable <- eventReactive(input$do, getMaltsShiny({unlist(input$Maltscbox)}))
  
  output$searchmalttable <- renderDataTable({
      MaltDirectory[row.names(getMaltsShiny(gsub("\\(", "\\\\(",unlist(input$Maltscbox)))),]
    })
  #proxy = dataTableProxy('searchmalttable')
  #
  #observeEvent(input$newmalt, {
  #  proxy %>% addRow(searchmalttable[nrow(searchmalttable), , drop = FALSE])
  #})

  
  #Hopbutton
  searchhop <- eventReactive(input$do2, gethopsShiny({gsub(" ","",unlist(strsplit(input$Hops, ",")))}))
  observeEvent(input$do2, {
    updateTabsetPanel(session, "tabset2", selected = "Hops")
  })

  output$Hoptext = renderUI({
    Hops <- searchhop()[,1]
    checkboxGroupInput("Hopscbox","Choose Hops", Hops)
  })
  
  searchhoptable <- eventReactive(input$do, gethopsShiny({unlist(input$Hopscbox)}))
  
  output$searchhoptable <- renderDataTable({HopDirectory[row.names(gethopsShiny(gsub("\\(", "\\\\(",unlist(input$Hopscbox)))),][c(1,23:25,28,33:35)]})
  
  #Yeastbutton
  searchyeast <- eventReactive(input$do3, getyeastsShiny({gsub(" ","",unlist(strsplit(input$Yeasts, ",")))}))
  
  observeEvent(input$do3, {
    updateTabsetPanel(session, "tabset2", selected = "Yeasts")
  })
  
  output$Yeasttext = renderUI({
    Yeasts <- as.character(searchyeast()[,2])
    checkboxGroupInput("Yeastscbox","Choose Yeasts", Yeasts)
  })
  
  searchyeasttable <- eventReactive(input$do, getyeastsShiny({unlist(input$Yeastcbox)}))
  
  output$searchyeasttable <- renderDataTable({yeasts[row.names(getyeastsShiny(gsub("\\(", "\\\\(",unlist(input$Yeastscbox)))),][-1]})
  

  #information
  #grain
  
  output$batch <- renderText({
    "test"
    #amountfunc(Target_Batch_L = input$slider, Grams_Grain, Boil_time_Hr, Target_Mash_Temp, Grain_Temp=NUL)
  })
  
  
  #amountfunc <- function(Target_Batch_L, Grams_Grain, Boil_time_Hr, Target_Mash_Temp, Grain_Temp=NULL)
  #"slider", "Target batch (L):", 0
  #"slider2", "Mash time (Min):", 0
  #"slider3", "Boil time (Min):", 0
  #"slider4", "Target mash temp (Ce
  #"slider5", "Grain temp (Cel):", 
  #"slider5", "Brewhouse efficiency
  
  set.seed(122)
  histdata <- rnorm(500)
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}

#add source boilmash
#add kg etc. converters
#add button
#add information tab


shinyApp(ui, server)







