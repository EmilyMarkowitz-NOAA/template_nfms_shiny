

ui.calculator <- function() {
  tabItem(
    tabName = "calculator",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      column(width = 12,
             
              box(h4(strong("Step 1: PROJECT INFORMATION"),
                     tags$style(type = "text/css", "#q_step1 {vertical-align: top;}"),
                     bsButton("q_step1", label = "", icon = icon("question-circle"), style = "color: #1f93d0; info", size = "extra-small")
              ),
              
              bsPopover(id = "q_step1", title = "Project Information",
                        content = paste0("If the user needs more room to enter their responce, they may expand the extents of the text boxes by dragging the icon in the lower right corner of the box."
                        ),
                        placement = "right",
                        trigger = "focus",
                        options = list(container = "body")
              ),
              
              textAreaInput(inputId = "Client",
                            label = "Project Title",
                            value = "", rows = 5),
              
              textAreaInput(inputId = "ProjectName",
                            label = "Project Contact",
                            value = "", rows = 5),
              
              textAreaInput(inputId = "ProjectDescription",
                            label = "Project/Source Information (Including Assumptions)",
                            value = "", rows = 10)
              )
      ),
      column(width = 12,
             
              
              ###R MARKDOWN REPORT
              box(
                downloadButton("report", "Generate Report"), 
                downloadButton("downloadData", "Download CSV File")
                )
      )
    )
  )
}