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


ui <-  dashboardPage(
  skin="black",
  # ==============================
  # =========== Header ===========
  # ==============================
dashboardHeader(
    title = tags$span(
      tags$img(src = "images/CurlyMouse_Big.png", height = "35px",
               style = "margin-right: 8px;"),
      "CurlyDB"
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
            imageOutput(outputId = "images/CurlyMouse_Big.png")
            
          ),
          
          
          box(
            textInput(inputId = "env_direc",
                      label="Type the path to your python environment here:"),
            verbatimTextOutput("env_path"),
            
            ))),

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
  output$env_path <- renderText({
      tryCatch(
        {use_python(input$env_direc)
          "Successfully Selected The Python Environment"}, 
        error= function(e){"Could not find the Python environment"})
    })
  output$plot1 <- renderPlot({
    req(input$GenPlot)  # This stops the code its in until the button is pressed
    
    pca_iris()
    })
  
  
  
  
}
  
  
  






# ============================================================================ #
# ======================== Construct Shiny Application ======================= #
# ============================================================================ #
shinyApp(ui, server)


