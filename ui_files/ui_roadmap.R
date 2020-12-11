ui.roadmap <- function() {
  tabItem(
    tabName = "roadmap",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line

      
      HTML("<h1>Hi this is heading<span class='glyphicon glyphicon-star'></span> Star</h1>"),
        h1("First level title"),
        h2("Second level title"),
        h3("Third level title"),
        h4("Fourth level title"),
        h5("Fifth level title"),
        h6("Sixth level title"), 
        p("This is paragraph text."), 
        br(), 
        strong("This is bold text."), 
        br(), 
        em("This is italic text."), 
        br(), 
        code("A <- 1 + 1"), 
        br(), 
        pre("Text ‘as is’ in a fixed width font."), 
        br(), 
        img(src="FISHERIES-Logo WEB ONLY.png", width = '35%'), 
        br(),
        a(href = 'https://www.fisheries.noaa.gov/', "link to NOAA Fisheries")

      )

  )
}