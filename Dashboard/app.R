# GUI packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyFiles)

# Plotting packages
library(ggplot2)



addResourcePath("images", "Images")
addResourcePath("db","../Database")

# Give source scripts.
source("Query.R")
source("Eval_Grouping.R")
source("../Scripts/Cluster/Distance.R")
source("../Scripts/Cluster/ClusterHeatMap.R")
source("../Scripts/Cluster/K_medoids.R")



phenotype_data_queried = FALSE
phenotye_dist_matrix_calculated   = FALSE



# ============================================================================ #
# ============================== User interface ============================== #
# ============================================================================ #
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
      menuItem("Phenotype_Analysis", tabName="Phenotype_Analysis", icon=icon("grain",lib = "glyphicon")),
      menuItem("Tissue_Analysis", tabName="Tissue_Analysis", icon=icon("eye-open",lib = "glyphicon"))
      
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
        fixedRow(
          box(
            title=h1("Welcome to CurlyDB!"),
            p("Instructions:"),
            p("1. (optional*) Select desired significance level and random seed"),
            p("2. Move to the desired analysis"),
                        
            tags$img(src = "images/CurlyMouse_Big_white.png", height = "235px",
                     style = "display: block; margin: 0 auto;"),
            
            style="display: block; margin: 0 auto;"
          ),
          box(
            h3("Random Seed"),
            "This ensures the clustering output is reproducible",
            br(),
            br(),
            numericInput("randomseed", label="Enter any number:", value = 67)
          ),
          box(
            h3("Significance Level"),
            "We recomend using IMPC's standard cutoff of P < 0.0001",
            tags$a(href="https://www.mousephenotype.org/help/data-analysis/statistical-analysis/", "(IMPC official website)"),
            br(),
            br(),
            numericInput("Signif_level", label="Enter Significance Level:", value = 0.0001)
          )
          
          ),
        
        fixedRow(
          box(
            title=h3("Database Schema"),            
            tags$img(src = "db/Schema.svg", height = "350px",
                     style = "display: block; margin: 0 auto;"),
            
            
          ),
          box(
            title= h3("Links:"),
            "Our Github:",
            tags$a(href="https://github.com/timomra/CurlyDB", "https://github.com/timomra/CurlyDB")
          )
        ) 
        ),
      # ==============================mv 
      # ==== Phenotype Analysis ======
      # ==============================
      tabItem(
        tabName = "Phenotype_Analysis",
        
        fluidRow(
          # Data Selection
          box(
            h3(strong("1. Data Selection")),
            width = 6,
            # select life stage for analysis
            checkboxGroupInput("Phenotype_life_stage_selection", label = h4("Mouse Life Stage (Select Multiple to Pool)"), 
                        choices = list("E9.5"="E9.5","E12.5" = "E12.5","E15.5" = "E15.5","E18.5" = "E18.5","Early adult"="Early adult","Middle aged adult"="Middle aged adult","Late adult"="Late adult"), 
                        selected = "Early adult"),
            
            # select grouping method for analysis.
            selectInput("Param_Grouping", label = h3("Parameter Grouping"),choices = list("No Grouping" = "nogroups"), selected = "nogroups"),
            actionButton("Phenotype_Query", strong("Query Database"), class="btn-success"),
            verbatimTextOutput("total_records"),
            verbatimTextOutput("total_genes"),
            verbatimTextOutput("total_params"),
            plotOutput("evaluate_groupings", height = 300, width = 450),
            
            ),
          
          
          box(
            # Do the distance calculation
            h3(strong("2. Distance Calculation")),
            width = 6,
            
            selectInput("DistanceChoice", label = ("Select Distance Metric:"), 
                        choices = list("Jaccard" = "jaccard", "Gower (Symetric Binary)"="gower","Euclidean"="euclidean"),
                        selected = "gower"),
            actionButton("Phenotype_distance", strong("Calculate Distances"), class="btn-warning"),
            verbatimTextOutput("distance_status"),
            
            plotOutput("matrix_heatmap",height = 500, width = 500),
            
            actionButton("Visualise_pheno_heatmap", strong("Visualise Distance Matrix"), class="btn-info"),
            
            
            
            )
            
          ),

        
        # Results Display
        fixedRow(
          height=5000,width=1500,
          box(
            h2(strong("3. Clustering")),
            plotOutput("phenoelbow_plot", height = 500, width = 500)
          ),
          box(
            h2(strong("4. Dimension Reduction Plot")),
            plotOutput("plot1", height = 500, width = 150)
          )
        ),
        
        
        fixedRow(
          # Control pannel for clustering
          box(
            actionButton("PhenoElbow",strong("Generate Elbow Plot"),class="btn-info"),
            
            actionButton("Cluster",strong("Run Clustering"),class="btn-danger"),
            
            selectInput("select", label = h3("Clustering"),
                        choices = list("k-Medoids" = "kmedoid"),
                        selected = 1),
            
            numericInput("k", label=h3("k"), value = 10)
          ),
          
          box(
            # Generate the dimension reduction plot.
            actionButton("GenPlot",strong("Generate Plot"),class="btn-danger"),
            
            
            selectInput("select", label = h3("Dimension Reduction"), 
                        choices = list("UMAP" = 1, "t-SNE" = 2), 
                        selected = 1),
          ),
        )
      ),
      
      # ==============================
      # ====== Tissue Analysis =======
      # ==============================
      tabItem(
        tabName = "Tissue_Analysis",
        
        # Data Selection
        fluidPage(
          box(
            width = 14,
            selectInput("select", label = h4("Mouse Life Stage"), 
                        choices = list("E9.5"=1,"E12.5" = 2,"E15.5" = 3,"E18.5" = 4,"Early adult"=5,"Middle aged adult"=6,"Late adult"=7), 
                        selected = 5),
            selectInput("select", label = h3("Distance Calculation"), 
                        choices = list("Jaccard" = 1, "Gower (Podani, 1999)"=2 ,"Bray–Curtis" = 3),
                        selected = 1),
          ),
        ),
        
        # Results Display
        fixedRow(
          height=5000,width=1500,
          box(
            h2("Dimension Reduction"),
            plotOutput("plot1", height = 500, width = 150)
          ),
          box(
            h2("Hierarchical Clustering + Heatmap"),
            plotOutput("plot1", height = 500, width = 150)
          )
          ),
        
        # Control Pannel
        fixedRow(
          box(
            # Control pannel for 
            actionButton("Cluster",strong("Cluster"),class="btn-danger"),
 
            selectInput("select", label = h3("Clustering"),
                        choices = list("k-means" = 1, "k-modes" = 2), 
                        selected = 1),
            numericInput("k", label=h3("k"), value = 5)
              ),
          
          box(
            # Generate the dimension reduction plot.
            actionButton("GenPlot",strong("Generate Plot"),class="btn-danger"),
            
            
            selectInput("select", label = h3("Dimension Reduction"), 
                        choices = list("UMAP" = 1, "t-SNE" = 2), 
                        selected = 1),
          )
        )
        )
      )
      )
)


# ============================================================================ #
# ================================== Server ================================== #
# ============================================================================ #
server <- function(input, output, session){
  # ======= Check and Change the level of Significance === #
  # ====================================================== #
  
  
  
  
  # ======= Query and evaluate Phenotype Data ============ #
  # ====================================================== #  
  observeEvent(
    input$Phenotype_Query, # <- Observes the reactive value of the actionbutton.
    # Runs when the reactive value changes.
    {
      # Take inputs from the UI and query the phenotypes data
      pheno_data <<- query_phenotype(
        life_stages = input$Phenotype_life_stage_selection,
        param_grouping = input$Param_Grouping)
      # Evaluate the groupings selected
      evaluations <- eval_phenogroups(pheno_data,input$Param_Grouping)
      
      # Output the phenotype data to the screen
      output$total_records <- renderText({paste("Total Records:",evaluations[1])})
      output$total_genes <- renderText({paste("Total Genes:",evaluations[2])})
      output$total_params <- renderText({paste("Total Parameters:",evaluations[3]) })
      output$evaluate_groupings <- renderPlot(evaluations[4])
      
      # Set this parameter to be TRUE (in the global namespace)
      # so the user can be allowed to do the next step.
      phenotype_data_queried <<- TRUE
    }
  )


  # = Calculate a distance matrix for the phenotype data = #
  # ====================================================== #
  observeEvent(
    input$Phenotype_distance,
    {
      if (phenotype_data_queried){
        # Tell the user that the calculation has started
        output$distance_status <- renderText({"Calculating distances..... this may take a few seconds"})
        
        # Do the calculation
        pheno_dist_matrix <<- pheno_distmatrix(
          in_pheno_data = pheno_data,
          distance = input$DistanceChoice,
          in_sig_level = input$Signif_level)
        
        # outputs
        output$distance_status <- renderText({"Distance Matrix Complete!"})
        phenotye_dist_matrix_calculated <<- TRUE
        
        
        
        

        }
      else{
        output$distance_status <- renderText({"Please Query the Database First!"})
      }
    }
  )
  # ======== Visualise phenotypes distance matrix ======== #
  # ====================================================== #
  observeEvent(
    input$Visualise_pheno_heatmap,
    {
      if (phenotye_dist_matrix_calculated){
        output$matrix_heatmap <- renderPlot(cluster_heatmap(pheno_dist_matrix))
        
      }else{
        print("Generate distance matrix first")
      }})
  
  
  # generate phenotype elbow plot.
  
  observeEvent(
    input$PhenoElbow,
    
    output$phenoelbow_plot <- renderPlot(optimise_k(in_d_matrix=pheno_dist_matrix, randomseed=input$randomseed) )
    
  )
  }

# ============================================================================ #
# ======================== Construct Shiny Application ======================= #
# ============================================================================ #
shinyApp(ui, server)


