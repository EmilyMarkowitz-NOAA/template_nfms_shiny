
ui.importdata <- function() {
  tabItem(
    tabName = "importdata",
    fluidRow(
      box(  
        selectInput("dataset", "Dataset", c("diamonds", "rock", "pressure", "cars"))
      ))
  )
}