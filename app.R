######################################################
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
# 
# This is a template for making an R Shiny app that will have most elements that be NMFS-comms OK
# 
# Writtiten by Emily Markowitz Emily.Markowitz@noaa.gov
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
# Platform: x86_64-w64-mingw32/x64 (64-bit)
######################################################


###########PACKAGES###################
# Need for running Shiny apps
library(shiny)

# Design
library(shinydashboard)
library(shinythemes)

# Use Java Script
library(shinyjs)
library(shinyBS)
require(V8)

# For table formatting
library(DT)
library(kableExtra)
library(formattable)

#Piping/operators which promote semantics
library(magrittr)
library(dplyr)

#Plotting
library(ggplot2)

#RMarkdown documents for reports 
library(knitr)
library(markdown) # #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application


##########SOURCE DATA####################
source("Reference.R") # Universal Documents
source("Functions.R") # App-specific files

##########USER INTERFACE###########
# Define UI for application that draws a histogram
ui <- shinyUI(
  
  #Allows JavaScript Files
  # useShinyjs(),
  # extendShinyjs(text = jsCode1),
  
  #Uses a pretty theme that is close to the NMFS theme
  # theme = shinytheme("flatly"),
  
  dashboardPage(
    header = dashboardHeader(title = "WiDS 2020"),
    sidebar = dashboardSidebar(sidebarMenu(id = "tabs", 
                  menuItem("Old Faithful Histogram", tabName = "hist")
      )
    ),
    body = dashboardBody(
                         tabItems(
                                  tabItem("hist", 
                                   box(sliderInput("bins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 30) ), 
                                   box(selectInput(inputId = "color", 
                                                   label = "Choose a color", 
                                                   choices = c("red", "blue", "black", "pink"), 
                                                   selected = "blue") ), #dropdown
                                   box(plotOutput("distPlot") ) ) ) )
  )
)


############SERVER#################
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #######* Images##########
  output$ImageFull <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_fisheries_small.png"))
    list(src = filename, 
         width = session$clientData$output_ImageFull_width,
         height = session$clientData$output_ImageFull_height
    )
  }, deleteFile = FALSE)
  
  output$Image <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_logo.gif"))
    list(src = filename, 
         width = session$clientData$output_Image_width,
         height = session$clientData$output_Image_height
    )
  }, deleteFile = FALSE)
  
  
  
  ########* Plots###########
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

