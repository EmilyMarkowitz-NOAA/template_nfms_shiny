
ui.import <- function() {
  tabItem(
    tabName = "import",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      box(  
        selectInput("dataset", "Dataset", c("faithful", "pressure", "cars")), 
        dataTableOutput('table')
        
      ))
  )
}