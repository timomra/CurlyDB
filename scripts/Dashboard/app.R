# GUI packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyFiles)

# Plotting packages
library(ggplot2)

# Import reticulate so we can use Python scripts
library(reticulate)




pca_iris <- function(){
  df<-iris[,1:4]
  source_python("../cluster/Iris_PCA.py")
  pca_data <- pca(df)
  
  ggplot(data=pca_data, aes(x=PC1,y=PC2)) +
    geom_point(
      shape=21, stroke=0.8, size = 3,
      fill="cornflowerblue",
      color="black",
      alpha = 0.8)+
    ggtitle("~/Onedrive/__PCA plot of Iris dataset R")

  }












# ============================================================================ #
# ============================== User interface ============================== #
# ============================================================================ #
ui <-  dashboardPage(
  skin="black",
  # ==============================
  # =========== Header ===========
  # ==============================
  dashboardHeader(title = "CurlyDB"),
  
  
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


