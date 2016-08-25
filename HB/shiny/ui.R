## ui.R ##
library(shinydashboard)

dashboardPage(
  dashboardHeader(dropdownMenuOutput("messageMenu")),
  dashboardSidebar(),
  dashboardBody()
)
