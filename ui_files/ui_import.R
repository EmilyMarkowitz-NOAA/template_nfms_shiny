
ui.import <- function() {
  tabItem(
    tabName = "import",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      h1("First level title"),
      h2("Second level title"),
      
      box(  
        selectInput("dataset", "Dataset", c("faithful", "pressure", "cars")), 
        dataTableOutput('table')
        
      ))
  )
}