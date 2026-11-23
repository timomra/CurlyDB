# GUI packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyFiles)

# Plotting packages
library(ggplot2)


# ============================================================================ #
# ============================== User interface ============================== #
# ============================================================================ #
addResourcePath("images", "Images")
addResourcePath("db","../Database")

ui <-  dashboardPage(
  skin="green",
  # ==============================
  # =========== Header ===========
  # ==============================
dashboardHeader(
    title = tags$span(   # <- using a tag to design the interface.
      tags$img(src = "images/CurlyMouse_Big.png", height = "35px",
               style = "margin-right: 8px;"),
      "CurlyDB",
    )
  ),
  
  
  # ==============================
  # =========== Sidebar ==========
  # ==============================  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="Home", icon=icon("book", lib = "glyphicon")),
      menuItem("PCA_plot", tabName="PCA_plot", icon=icon("tree"))
    )),
  
  
  # ==============================
  # =========== Body =============
  # ==============================
  dashboardBody(
    tabItems(
      
      # ==============================
      # ========= Home tab ===========
      # ==============================
      tabItem(
        tabName = "Home",
        fluidRow(
          box(
            title="Welcome to CurlyDB!",
            p("Instructions:"),
            p(paste0(readLines("instructions.txt"))),
            
            tags$img(src = "images/CurlyMouse_Big_white.png", height = "235px",
                     style = "display: block; margin: 0 auto;"),
            
            style="display: block; margin: 0 auto;"
            
            
          ),
          box(
            title="Database Schema",            
            tags$img(src = "db/Schema.svg", height = "250px",
                     style = "display: block; margin: 0 auto;"),
            
            
          )
          )),

      # ==============================
      # ========= PCA plot ===========
      # ==============================
      tabItem(
        tabName = "PCA_plot",
        fluidRow(
          box(actionButton("GenPlot","Generate Plot")),
          box(
            plotOutput("plot1", height = 250)
            
            
          )
        )
        
      )
      )))



# ============================================================================ #
# ================================== Server ================================== #
# ============================================================================ #
server <- function(input, output, session){
  # ======= Start the Correct Python environment ========= #
  # ====================================================== #
  ## NOTE: if we do go with this approach, I'll make this a button so it runs more efficiently.

  output$plot1 <- renderPlot({
    req(input$GenPlot)  # This stops the code its in until the button is pressed
    
    pca_iris()
    })
  
  
  
  
}
  
  
  






# ============================================================================ #
# ======================== Construct Shiny Application ======================= #
# ============================================================================ #
shinyApp(ui, server)


