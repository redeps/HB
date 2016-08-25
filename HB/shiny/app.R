#install.packages("shinydashboard")
#install.packages("shinytheme")
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
##install.packages("directlabels")
#library(directlabels)
#library(grid)
##install.packages("extrafont")
#library(extrafont)
##install.packages("useful")
#library(useful)
##font_import() # Import all fonts
##fonts() # Print list of all fonts
##install.packages("jpeg")
#library(jpeg)
#library(plotly)


setwd("S:/Peder/training/R/HB")

source(paste0(getwd(),"/maltsdirectory.R"))
source(paste0(getwd(),"/hopsdirectory.R"))
source(paste0(getwd(),"/yeastdirectory.R"))

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
    menuItem("Widgets", tabName = "widgets", icon = icon("th"),
             badgeLabel = "new", badgeColor = "green"),
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
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tabBox(
                  title = "First tabBox",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "400px",
                  tabPanel("Tab1",textInput("Malts",label = h3("Malts:"), value = ""),
                           textInput("Hops",label = h3("Hops:"), value = ""),
                           textInput("Yeasts",label = h3("Yeasts:"), value = "")),
                  tabPanel("Tab2",plotOutput("plot1", height = 350))
                  ),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              fluidRow(
                tabBox(
                  title = "Choices", 
                  id = "Choices", width = 12,
                  tabPanel("Malts",
                            actionButton("do", "Show Choices"),
                            dataTableOutput("Malttext")),
                  tabPanel("Hops",
                           actionButton("do2", "Show Choices"),
                           dataTableOutput("Hoptext")),
                  tabPanel("Yeasts",
                           actionButton("do3", "Show Choices"),
                           dataTableOutput("Yeasttext"))
                )
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                tabBox(
                  title = "Table",
                  id = "tabset2", height = "400px", width = 12,
                  tabPanel("Tab1b")
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
  output$Malttext = renderDataTable({
    searchmalt()
  })
  
  #Hopbutton
  searchhop <- eventReactive(input$do2, gethopsShiny({gsub(" ","",unlist(strsplit(input$Hops, ",")))}))
  output$Hoptext = renderDataTable({
    searchhop()
  })
  
  #Yeastbutton
  searchyeast <- eventReactive(input$do3, getyeastsShiny({gsub(" ","",unlist(strsplit(input$Yeasts, ",")))}))
  output$Yeasttext = renderDataTable({
    searchyeast()
  })

  
  set.seed(122)
  histdata <- rnorm(500)

  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}




shinyApp(ui, server)



