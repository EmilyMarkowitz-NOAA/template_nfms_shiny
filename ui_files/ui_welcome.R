
ui.welcome <- function() {
  
  
  
  
  tabItem(
    tabName = "welcome",
    
    # req(credentials()$user_auth),
    
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      h1("First level title"),
      h2("Second level title"),
    
      column(
      
        width = 12,
        tags$p(glue("Your permission level is: {user_info()$permissions}. 
                       Your data is: {ifelse(user_info()$permissions == 'admin', 'Starwars', 'Storms')}.")),
        box(width = NULL, status = "primary",
            title = ifelse(user_info()$permissions == 'admin', "Starwars Data", "Storms Data"),
            DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  )
}