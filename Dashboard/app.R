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
source("../Scripts/Cluster/DimRed.R")

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
      menuItem("Phenotype Analysis", tabName="Phenotype_Analysis", icon=icon("grain",lib = "glyphicon")),
      menuItem("Tissue Analysis", tabName="Tissue_Analysis", icon=icon("eye-open",lib = "glyphicon")),
      menuItem("Manhattan Plot", tabName="Manhattan_Plot", icon=icon("equalizer",lib = "glyphicon"))
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
                        choices = list("Gower (Asymmetric Binary)" = "gower_Asym", "Gower (Symmetric Binary)"="gower_Sym"),
                        selected = "Gower (Symmetric Binary)"),
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
            plotOutput("pheno_dimred_plot", height = 500, width = 600)
          )
        ),
        
        fixedRow(
          # Control pannel for clustering
          box(
            actionButton("run_pheno_clustering",strong("Run Clustering"),class="btn-danger"),
            conditionalPanel(
                  condition = "input.select_pheno_clustering == 'kmedoid'", 
                  actionButton("PhenoElbow",strong("Generate Elbow Plot"),class="btn-info"),
            
            # Run the clustering
            selectInput("select_pheno_clustering", label = h3("Clustering"),
                        choices = list("k-Medoids" = "kmedoid"),
                        selected = 1),
            
            numericInput("k_pheno", label=h3("k"), value = 10)
          )),
          
          box(
            # Generate the dimension reduction plot.
            actionButton("Generate_pheno_dimred_plot",strong("Generate Plot"),class="btn-danger"),
            checkboxInput("pheno_include_clustering", "Include clustering labels?",TRUE),
            
            selectInput("pheno_dimred", label = h3("Dimension Reduction"), 
                        choices = list("UMAP" = "umap", "t-SNE" = "tsne"), 
                        selected = "UMAP"),
            
            # Allow users to select the hyperparameters perplexity OR n neighbours 
            # depending on whether t-SNE or UMAP were selected. 
            # While other hyperparameters exist for these algorithms, these were 
            # included as they can allow the user to select whether they are more 
            # interested in local or global structure depending on the value 
            # for k they picked.
            conditionalPanel(
              condition = "input.pheno_dimred == 'tsne'",
              numericInput("hyper_param", label=h3("t-SNE perplexity"), value = 30)
            ),
            conditionalPanel(
              condition = "input.pheno_dimred == 'umap'",
              numericInput("hyper_param", label=h3("UMAP n neighbours"), value = 30)
            )
          ),
        ),
        fixedRow(
          height=5000,width=1500,
          
          box(
            
            selectInput("select_cluster", label = "Select cluster:", choices = NULL)
            
          ),
          
          box(

            
          )
          
          
          
          
          
          
        )
        
        
      ),
      
      
      
      
      
      # ==============================
      # ====== Tissue Analysis =======
      # ==============================
      tabItem(
        tabName = "Tissue_Analysis",
        



          )
)))


# ============================================================================ #
# ================================== Server ================================== #
# ============================================================================ #
server <- function(input, output, session){
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
    output$phenoelbow_plot <- renderPlot(optimise_k(in_d_matrix=pheno_dist_matrix, randomseed=input$randomseed))
  )
  
  # ============== Run phenotype clustering ============== #
  # ====================================================== #
  observeEvent(
    input$run_pheno_clustering,
    {
      clusters <<- clustering(
        in_d_matrix = pheno_dist_matrix,
        randomseed=input$randomseed,
        cluster_method = input$select_pheno_clustering,
        k= input$k_pheno
        
        
      )
      
      
      updateSelectInput(session, "select_cluster",
                        choices = seq_len(input$k_pheno),
                        selected = 1)
      
      
    }
    
    
  )
  
  
  
  # ==== Visualise phenotypes dimension reduction plot === #
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
    input$Generate_pheno_dimred_plot,
    {
    output$pheno_dimred_plot <- renderPlot(
      
      plot_dimred(embeddings_df = get_embeddings(
        
        in_dist_matrix = pheno_dist_matrix,
        random_seed=input$randomseed,
        dimred_method = input$pheno_dimred,
        hyper_param = input$hyper_param),
        
        include_clusters=input$pheno_include_clustering,
        cluster_df = clusters
      )
      )})
  
  }

  


# ============================================================================ #
# ======================== Construct Shiny Application ======================= #
# ============================================================================ #
shinyApp(ui, server)


