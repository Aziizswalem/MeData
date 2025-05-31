library(shiny)
library(DT)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(shinythemes)
library(ggrepel)
library(plotly)
library(colourpicker)
library(viridis)
library(RColorBrewer)
library(shinyWidgets)
library(rhandsontable)
library(gganatogram)
library(pheatmap)
library(heatmaply)
library(reactable)
library(paletteer)
library(gridExtra)
library(fontawesome)
library(shinyjs)
library(colourpicker)
library(stats)
library(goftest)
library(RNifti)
library(markdown)
library(devtools)
library(knitr)
library(pander)
library(xfun)
library(tseries)
source("lazyr.R")
source("interactive.R")
source("animation.R")
library(rsconnect)

read_img_as_array <- function(path) {
  img_raw <- RNifti::readNifti(path)
  if (length(dim(img_raw)) == 3) return(img_raw[,,])
  return(img_raw[,,,])
}


ui <- navbarPage( useShinyjs(),
        
  title = div(
    h1("MeData", style = "font-size: 38px; margin-top: -5px; text-align: center;"), 
    HTML('<img src="essai-removebg-preview.png" width="85" style="position: absolute; top: -25px; right: 0;">')
  ),
  theme = shinytheme("cosmo"),  # Set a default theme
  
  tags$head(
    tags$style(
      ".dark-mode {
        background-color: #1e1e1e;
        color: #fff;
      }
      .dark-mode .navbar {
        background-color: #333;
      }
      .dark-mode .navbar-nav .nav-link {
        color: #fff;
      }
      .dark-mode .navbar-nav .nav-link.active {
        color: #007bff;
      }
      .dark-mode .navbar-toggler-icon {
        background-color: #fff;
      }
      /* Add more styles for dark mode as needed */
      .light-mode {
        background-color: #fff;
        color: #333;
      }
      .light-mode .navbar {
        background-color: #f8f9fa;
      }
      .light-mode .navbar-nav .nav-link {
        color: #333;
      }
      .light-mode .navbar-nav .nav-link.active {
        color: #007bff;
      }
      .light-mode .navbar-toggler-icon {
        background-color: #333;
      }
      /* Add more styles for light mode as needed */
    "
    ),
    tags$script(
      HTML('
        $(document).ready(function(){
          $("#dark_mode").change(function(){
            if($(this).is(":checked")){
              $("body").addClass("dark-mode");
            } else {
              $("body").removeClass("dark-mode");
            }
          });
        });
      ')
    )
  ),
  
  tabPanel(
    "Data Overview",
    icon = icon("table"),
    fluidRow(
      column(10, align = "left", fileInput("file", "Choose a file")),
      dataTableOutput("data_summary")
    )
  ),
  tabPanel(
    "Cleaning Data",
    icon = icon("broom"),
    sidebarLayout(
      sidebarPanel(
        checkboxInput("fill_missing_values", "Fill Missing Values", FALSE),
        conditionalPanel(
          condition = "input.fill_missing_values",
          selectInput("fill_with", "Fill with:", choices = c("0", "Mean", "Median", "Max"))
        ),
        br(),
        checkboxInput("delete_rows", "Delete Rows with Missing Values", FALSE),
        br(),
        actionButton("clean_data_button", "Clean Data", class = "btn-primary")
      ),
      mainPanel(
        DTOutput("missing_values_table")
      )
    )
  ),
  tabPanel(
    "Descriptive Statistics",
    icon = icon("chart-bar"),
    tabsetPanel(
      tabPanel(
        "Overview Statistics",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("overview_variables", "Select Variables:", choices = NULL)
          ),
          mainPanel(
            dataTableOutput("overview_statistics_table")
          )
        )
      ),
      tabPanel("Barplot",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("barplot_variables", "Select Variables:", choices = NULL),
                   checkboxInput("show_counts", "Show Counts", FALSE),
                   textInput("color_code_barplot", "Color:", placeholder = "#Color Code")
                 ),
                 mainPanel(
                   plotOutput("barplot")
                 )
               )
      ),
      tabPanel("Histogram",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("histogram_variables", "Select Variable:", choices = NULL , multiple = FALSE),
                   sliderInput("histogram_bins", "Number of Bins:", min = 1, max = 100, value = 10),
                   textInput("color_code_histogram", "Color:", placeholder = "#Color Code")
                 ),
                 mainPanel(
                   plotOutput("histogram")
                 )
               )
      ),
      tabPanel("Pie Chart",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("piechart_variables", "Select a Variable:",choices=NULL)
                 ),
                 mainPanel(
                   plotlyOutput("pie_chart")
                 )
               )
      ),
      tabPanel("Boxplot",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("boxplot_variables_ui"),
                   checkboxInput("color_code_boxplot", "Custom Color", FALSE),
                   conditionalPanel(
                     condition = "input.color_code_boxplot",
                     textInput("color_code_boxplot_input", "Color Code:", placeholder = "#RRGGBB")
                   )
                 ),
                 mainPanel(
                   plotlyOutput("boxplot")
                 )
               )
      ),
      tabPanel("Density Plot", 
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("densityplot_variables_ui")
                 ),
                 mainPanel(
                   plotOutput("density_plot")
                 )
               )
      ),
      tabPanel("Scatter Plot", 
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("scatterplot_variables", "Select Variables:", choices = NULL)
                 ),
                 mainPanel(
                   plotlyOutput("scatter_plot")
                 )
               )
      ),

      tabPanel(
        "Correlation Analysis",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("correlation_variables", "Select Variables:", choices = NULL),
            selectInput("correlation_method", "Select Correlation Method:",
                        choices = c("Pearson's", "Spearman's", "Kendall's", "Cramer's V", "Point-biserial"),
                        selected = "Pearson's")
          ),
          mainPanel(
            plotOutput("correlation_heatmap", height = "500px"), # Adjust height for better visibility
            br(),
            h4("Correlation Strength Guide"), # Adjust header size
            reactableOutput("table1", height = "350px"), # Adjust height for better visibility
            br(),
            h4("Type of Relationship Guide"), # Adjust header size
            reactableOutput("table2", height = "350px") # Adjust height for better visibility
          )
        )
      )
      
       
    )
  ),
      # New tab for inferential statistics
  tabPanel(
    "Inferential Statistics",
    icon = icon("infinity"),
    tabsetPanel(
        tabPanel("How it simply works",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "inference",
              label = "Inference for:",
              choices = c("one mean", "two means", "two means (paired samples)", "one proportion", "two proportions", "one variance", "two variances"),
              multiple = FALSE,
              selected = "one mean"
            ),
            hr(),
            conditionalPanel(
              condition = "input.inference == 'one mean'",
              textInput("sample_onemean", "Sample", value = "0.9, -0.8, 1.3, -0.3, 1.7", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              hr(),
              checkboxInput("popsd_onemean", "Variance of the population is known", FALSE),
              conditionalPanel(
                condition = "input.popsd_onemean == 1",
                numericInput("sigma2_onemean", "\\(\\sigma^2 = \\)",
                             value = 1, min = 0, step = 1
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'two means'",
              textInput("sample1_twomeans", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twomeans", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              hr(),
              conditionalPanel(
                condition = "input.popsd_twomeans == 0",
                radioButtons(
                  inputId = "var.equal",
                  label = "Assuming",
                  choices = c(
                    "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                    "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                  )
                )
              ),
              checkboxInput("popsd_twomeans", "Variance of the populations are known", FALSE),
              conditionalPanel(
                condition = "input.popsd_twomeans == 1",
                numericInput("sigma21_twomeans", "\\(\\sigma^2_1 = \\)",
                             value = 1, min = 0, step = 1
                ),
                numericInput("sigma22_twomeans", "\\(\\sigma^2_2 = \\)",
                             value = 1, min = 0, step = 1
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'two means (paired samples)'",
              textInput("sample1_twomeanspaired", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twomeanspaired", "Sample 2", value = "0.8, -0.9, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              hr(),
              checkboxInput("popsd_twomeanspaired", "\\( \\sigma^2_D \\) is known", FALSE),
              conditionalPanel(
                condition = "input.popsd_twomeanspaired == 1",
                numericInput("sigma2_twomeanspaired", "\\(\\sigma^2_D = \\)",
                             value = 1, min = 0, step = 1
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'one proportion'",
              tags$b("Sample size"),
              numericInput("n_oneprop", "\\(n = \\)",
                           value = 30, min = 0, step = 1
              ),
              hr(),
              radioButtons(
                inputId = "propx_oneprop",
                label = NULL,
                choices = c(
                  "Proportion of success \\(\\hat{p}\\)" = "prop_true",
                  "Number of successes \\(x\\)" = "prop_false"
                )
              ),
              conditionalPanel(
                condition = "input.propx_oneprop == 'prop_true'",
                tags$b("Proportion of success"),
                numericInput("p_oneprop", "\\(\\hat{p} = \\)",
                             value = 0.2, min = 0, max = 1, step = 0.01
                )
              ),
              conditionalPanel(
                condition = "input.propx_oneprop == 'prop_false'",
                tags$b("Number of successes"),
                numericInput("x_oneprop", "\\(x = \\)",
                             value = 10, min = 0, step = 1
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'two proportions'",
              tags$b("Sample size 1"),
              numericInput("n1_twoprop", "\\(n_1 = \\)",
                           value = 30, min = 0, step = 1
              ),
              tags$b("Sample size 2"),
              numericInput("n2_twoprop", "\\(n_2 = \\)",
                           value = 30, min = 0, step = 1
              ),
              hr(),
              radioButtons(
                inputId = "propx_twoprop",
                label = NULL,
                choices = c(
                  "Proportion of success \\(\\hat{p}\\)" = "prop_true",
                  "Number of successes \\(x\\)" = "prop_false"
                )
              ),
              conditionalPanel(
                condition = "input.propx_twoprop == 'prop_true'",
                tags$b("Proportion of success"),
                numericInput("p1_twoprop", "\\(\\hat{p}_1 = \\)",
                             value = 0.2, min = 0, max = 1, step = 0.01
                ),
                numericInput("p2_twoprop", "\\(\\hat{p}_2 = \\)",
                             value = 0.3, min = 0, max = 1, step = 0.01
                )
              ),
              conditionalPanel(
                condition = "input.propx_twoprop == 'prop_false'",
                tags$b("Number of successes"),
                numericInput("x1_twoprop", "\\(x_1 = \\)",
                             value = 10, min = 0, step = 1
                ),
                numericInput("x2_twoprop", "\\(x_2 = \\)",
                             value = 12, min = 0, step = 1
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'one variance'",
              textInput("sample_onevar", "Sample", value = "0.9, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 795, 810, 775, 781, 803, 823, 780, etc.")
            ),
            conditionalPanel(
              condition = "input.inference == 'two variances'",
              textInput("sample1_twovar", "Sample 1", value = "0.9, -0.8, 0.1, -0.3, 0.2, 0.7, -0.8, 0.1, -0.3, 0.2", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
              textInput("sample2_twovar", "Sample 2", value = "0.4, -0.3, -0.1, 0.4, 0.1, 0.2, -0.1, -0.1, 0.4, 0.1", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc.")
            ),
            hr(),
            tags$b("Null hypothesis"),
            conditionalPanel(
              condition = "input.inference == 'one mean'",
              sprintf("\\( H_0 : \\mu = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'two means'",
              sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'two means (paired samples)'",
              sprintf("\\( H_0 : \\mu_D = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'one proportion'",
              sprintf("\\( H_0 : p = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'two proportions'",
              sprintf("\\( H_0 : p_1 - p_2 = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'one variance'",
              sprintf("\\( H_0 : \\sigma^2 = \\)")
            ),
            conditionalPanel(
              condition = "input.inference == 'two variances'",
              sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)")
            ),
            conditionalPanel(
              condition = "input.inference != 'two variances'",
              numericInput("h0",
                           label = NULL,
                           value = 0.1, step = 0.1
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'two variances'",
              br()
            ),
            conditionalPanel(
              condition = "input.inference == 'two proportions'",
              checkboxInput("pooledstderr_twoprop", "Use pooled standard error", FALSE)
            ),
            conditionalPanel(
              condition = "input.inference != 'two variances'",
              radioButtons(
                inputId = "alternative",
                label = "Alternative",
                choices = c(
                  "\\( \\neq \\)" = "two.sided",
                  "\\( > \\)" = "greater",
                  "\\( < \\)" = "less"
                )
              )
            ),
            conditionalPanel(
              condition = "input.inference == 'two variances'",
              radioButtons(
                inputId = "alternative_twovar",
                label = "Alternative",
                choices = c(
                  "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
                  "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater",
                  "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less"
                )
              )
            ),
            hr(),
            sliderInput("alpha",
                        "Significance level \\(\\alpha = \\)",
                        min = 0.01,
                        max = 0.20,
                        value = 0.05
            ),
            hr(),
           
          ),
          
          mainPanel(
            conditionalPanel(
              condition = "input.inference == 'one mean'",
              uiOutput("results_onemean")
            ),
            conditionalPanel(
              condition = "input.inference == 'two means'",
              uiOutput("results_twomeans")
            ),
            conditionalPanel(
              condition = "input.inference == 'two means (paired samples)'",
              uiOutput("results_twomeanspaired")
            ),
            conditionalPanel(
              condition = "input.inference == 'one proportion'",
              uiOutput("results_oneprop")
            ),
            conditionalPanel(
              condition = "input.inference == 'two proportions'",
              uiOutput("results_twoprop")
            ),
            conditionalPanel(
              condition = "input.inference == 'one variance'",
              uiOutput("results_onevar")
            ),
            conditionalPanel(
              condition = "input.inference == 'two variances'",
              uiOutput("results_twovar")
            ),
            br(),
            br(),
            plotOutput("plot"),
            br(),
            br()
          )
        )
      
        ),
        tabPanel(
          "Statistical Tests",
          sidebarLayout(
            sidebarPanel(
              selectInput("parametric_or_nonparametric", "Select Test Type:",
                          choices = c("Parametric Test", "Non-parametric Test")),
              conditionalPanel(
                condition = "input.parametric_or_nonparametric == 'Parametric Test'",
                selectInput("parametric_test_type", "Select Parametric Test Type:",
                            choices = c("Normality Test", "Correlation Test", "Independence Test")),
                conditionalPanel(
                  condition = "input.parametric_test_type == 'Normality Test'",
                  selectInput("parametric_normal_test_variable", "Select Variable for Normal Test:", choices = NULL),
                  actionButton("run_parametric_normal_test_button", "Run Normality Test", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.parametric_test_type == 'Correlation Test'",
                  selectInput("parametric_correlation_test_variable1", "Select Variable 1:", choices = NULL),
                  selectInput("parametric_correlation_test_variable2", "Select Variable 2:", choices = NULL),
                  actionButton("run_parametric_correlation_test_button", "Run Correlation Test", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.parametric_test_type == 'Independence Test'",
                  selectInput("parametric_independence_test_variables", "Select Variables:", choices = NULL, multiple = TRUE),
                  actionButton("run_parametric_independence_test_button", "Run Independence Test", class = "btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.parametric_or_nonparametric == 'Non-parametric Test'",
                selectInput("nonparametric_test_type", "Select Non-parametric Test Type:",
                            choices = c("Normality Test", "Correlation Test", "Independence Test")),
                conditionalPanel(
                  condition = "input.nonparametric_test_type == 'Normality Test'",
                  selectInput("nonparametric_normal_test_variable", "Select Variable:", choices = NULL),
                  actionButton("run_nonparametric_normal_test_button", "Run Normality Test", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.nonparametric_test_type == 'Correlation Test'",
                  selectInput("nonparametric_correlation_test_variable1", "Select Variable 1:", choices = NULL),
                  selectInput("nonparametric_correlation_test_variable2", "Select Variable 2:", choices = NULL),
                  actionButton("run_nonparametric_correlation_test_button", "Run Correlation Test", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.nonparametric_test_type == 'Independence Test'",
                  selectInput("nonparametric_independence_test_variables", "Select Variables:", choices = NULL, multiple = TRUE),
                  actionButton("run_nonparametric_independence_test_button", "Run Independence Test", class = "btn-primary")
                )
              )
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Parametric Test Results",
                         conditionalPanel(
                           condition = "input.parametric_test_type == 'Normality Test'",
                           br(),
                           br(),
                           tableOutput("parametric_normal_test_result"),
                           br()
                           ),
                         conditionalPanel(
                           condition = "input.parametric_test_type == 'Correlation Test'",
                           tableOutput("parametric_correlation_test_result")
                         ),
                         conditionalPanel(
                           condition = "input.parametric_test_type == 'Independence Test'",
                           tableOutput("parametric_independence_test_result")
                         )
                ),
                tabPanel("Non-parametric Test Results",
                         conditionalPanel(
                           condition = "input.nonparametric_test_type == 'Normality Test'",
                           tableOutput("nonparametric_normal_test_result")
                         ),
                         conditionalPanel(
                           condition = "input.nonparametric_test_type == 'Correlation Test'",
                           tableOutput("nonparametric_correlation_test_result")
                         ),
                         conditionalPanel(
                           condition = "input.nonparametric_test_type == 'Independence Test'",
                           tableOutput("nonparametric_independence_test_result")
                         )
                )
                
              ),
              br(),
              h4("Support"), # Adjust header size
              reactableOutput("table3", height = "350px"), # Adjust height for better visibility
              br(),
            )
          )
        )
    ))
  ,
  tabPanel(
    "Linear Regression",
    icon = icon("tachometer-alt"),
    sidebarLayout(
      sidebarPanel(
        selectInput("linear_regression_variable1", "Select x variable:", choices = NULL),
        selectInput("linear_regression_variable2", "Select y variable:", choices = NULL),
        checkboxInput("se", "Add confidence interval around the regression line", TRUE),
        
        actionButton("run_regression_button", "Run Regression", class = "btn-primary"),
      ),
      mainPanel(
        br(),
        tags$b("Data:"),
        br(),
        br(),
        DT::dataTableOutput("tbl"),
        br(),
        uiOutput("data"),
        br(),
        tags$b("Compute parameters by hand:"),
        br(),
        br(),
        uiOutput("by_hand"),
        br(),
        tags$b("Compute parameters in R:"),
        br(),
        br(),
        verbatimTextOutput("summary"),
        br(),
        tags$b("Regression plot:"),
        br(),
        br(),
        uiOutput("results"),
        plotlyOutput("plot1"),
        br(),
        tags$b("Interpretation:"),
        br(),
        br(),
        uiOutput("interpretation"),
        br(),
        tags$b("Assumptions:"),
        plotOutput("assumptions"),
        br(),
        br()
      )
    )
  )
  ,
  
  tabPanel(
    "Medical Observations",
    icon = icon("user-doctor"),
    tabsetPanel(
      tabPanel(
        "Human Anatogram Plot",
        
        tableOutput("Human_Anatomy_Plot"),
        uiOutput('Species'),
        checkboxInput("showOutline", "Show outline", value = TRUE, width = NULL),
        uiOutput('fill'),
        pickerInput("col", "Outline colour", "#a6bddb"),
        uiOutput("valueColour"),
        checkboxInput("reverseId", "reverse colour palette", value = TRUE, width = NULL),
        uiOutput('Organs'),
        textInput("height", label = "Height in cm", value = "15"),
        textInput("ggtitle", label = "Plot title", value = ""),
        uiOutput("plot.ui"),
        sidebarPanel(
          selectInput("medical_issue", "Select Medical Disease:", 
                      choices = c("Normal", "Cancer", "Alzheimer", "Parkinson"),
                      selected = "Normal")
        ),
        mainPanel(
          plotOutput("medical_comparison_plot")
        ),
        tabPanel("Species_Key", rHandsontableOutput("mytable2"), position = "bottom")
      ),
      tabPanel(
        "MRI Observation",
        fluidRow(
          column(3, selectInput(
            "demo_dt", "Choose a Demo Data", choices = c(
              "3D Brain MRI" = "data/3d_t1w.nii.gz",
              "4D Brain fMRI" = "data/4d_fmri.nii.gz"
            )
          )),
          column(1, h5("Or"), class = "text-center", style = "padding-top: 15px;"),
          column(4, fileInput("your_dt", "Upload a .nii .nii.gz")),
          column(1, h2("|"), class = "text-center", style = "margin-top: -5px; "),
          column(3, shinyWidgets::switchInput(
            "interactive", "Interactive", onStatus = "success"
          ), style = "padding-top: 25px;")
        ),
        uiOutput("raster_panel")
      )
    )
  )
  
  
)
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 600*1024^2)
  
  
  data <- reactiveVal(NULL)
  histogram_variables <- reactiveVal(NULL)
  piechart_variable <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    data(read.csv(input$file$datapath, stringsAsFactors = FALSE))
    updateCheckboxGroupInput(session, "overview_variables", choices = names(data()[sapply(data(), is.numeric)]))  # Add this line
    updateCheckboxGroupInput(session, "barplot_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "histogram_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateSelectInput(session, "piechart_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateCheckboxGroupInput(session, "boxplot_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateCheckboxGroupInput(session, "scatterplot_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateCheckboxGroupInput(session, "densityplot_variables", choices = names(data()[sapply(data(), is.numeric)]))
    updateCheckboxGroupInput(session, "correlation_variables", choices = names(data()[sapply(data(), is.numeric)]))  # Add this line
    updateSelectInput(session, "normal_test_variable", choices = names(data()[sapply(data(), is.numeric)]))
    # Update choices for Fisher test (between quantitative variables)
    updateSelectInput(session, "fisher_test_variable1", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "fisher_test_variable2", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "anova_variables", choices = names(data())[sapply(data(), is.numeric)])
    #ks
    updateSelectInput(session, "ks_test_variable", "Select Variable:", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "shapiro_test_variable", "Select Variable:", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "linear_regression_variable1", choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "linear_regression_variable2", choices = names(data())[sapply(data(), is.numeric)])
    
  })
  
  # Update selectInput for parametric normality test
  observe({
    updateSelectInput(session, "parametric_normal_test_variable", 
                      choices = names(data())[sapply(data(), is.numeric)])
  })
  
  # Update selectInput for parametric correlation test
  observe({
    updateSelectInput(session, "parametric_correlation_test_variable1", 
                      choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "parametric_correlation_test_variable2", 
                      choices = names(data())[sapply(data(), is.numeric)])
  })
  
  # Update selectInput for parametric independence test
  observe({
    updateSelectInput(session, "parametric_independence_test_variables", 
                      choices = names(data())[sapply(data(), is.numeric)], selected = NULL)
  })
  
  # Update selectInput for non-parametric normality test
  observe({
    updateSelectInput(session, "nonparametric_normal_test_variable", 
                      choices = names(data())[sapply(data(), is.numeric)])
  })
  
  # Update selectInput for non-parametric correlation test
  observe({
    updateSelectInput(session, "nonparametric_correlation_test_variable1", 
                      choices = names(data())[sapply(data(), is.numeric)])
    updateSelectInput(session, "nonparametric_correlation_test_variable2", 
                      choices = names(data())[sapply(data(), is.numeric)])
  })
  
  # Update selectInput for non-parametric independence test
  observe({
    updateSelectInput(session, "nonparametric_independence_test_variables", 
                      choices = names(data())[sapply(data(), is.numeric)], selected = NULL)
  })
  
  # Server function for parametric normality test
  observeEvent(input$run_parametric_normal_test_button, {
    variable <- data()[[input$parametric_normal_test_variable]]
    jb_test <- jarque.bera.test(variable)
    output$parametric_normal_test_result <- renderTable({
      data.frame(
        Statistic = jb_test$statistic,
        p_value = jb_test$p.value
      )
    })
  })
  
  # Server function for parametric correlation test
  observeEvent(input$run_parametric_correlation_test_button, {
    variable1 <- data()[[input$parametric_correlation_test_variable1]]
    variable2 <- data()[[input$parametric_correlation_test_variable2]]
    pearson_test <- cor.test(variable1, variable2, method = "pearson")
    output$parametric_correlation_test_result <- renderTable({
      data.frame(
        Estimate = pearson_test$estimate,
        p_value = pearson_test$p.value
      )
    })
  })
  
  # Server function for parametric independence test
  observeEvent(input$run_parametric_independence_test_button, {
    variables <- input$parametric_independence_test_variables
    if (length(variables) >= 2) {
      anova_test <- aov(formula = as.formula(paste(variables, collapse = " ~ ")), data = data())
      output$parametric_independence_test_result <- renderTable({
        anova_summary <- summary(anova_test)
        if ("Between Groups" %in% rownames(anova_summary)) {
          anova_table <- data.frame(
            Df = c(anova_summary$"Between Groups"$Df, anova_summary$"Error: Within"$Df),
            Sum_Sq = c(anova_summary$"Between Groups"$`Sum Sq`, anova_summary$"Error: Within"$`Sum Sq`),
            Mean_Sq = c(anova_summary$"Between Groups"$`Mean Sq`, anova_summary$"Error: Within"$`Mean Sq`),
            F_value = c(anova_summary$"Between Groups"$`F value`, NA),
            Pr_F = c(anova_summary$"Between Groups"$`Pr(>F)`, NA)
          )
        } else {
          output$parametric_independence_test_result <- renderText("ANOVA test cannot be performed.")
          return(NULL)
        }
        rownames(anova_table) <- NULL  # Remove row names
        anova_table
      })
    } else {
      output$parametric_independence_test_result <- renderText("Please select at least two variables for the independence test.")
    }
  })
  
  
  
  # Server function for non-parametric normality test
  observeEvent(input$run_nonparametric_normal_test_button, {
    variable <- data()[[input$nonparametric_normal_test_variable]]
    ks_test <- ks.test(variable, "pnorm")
    output$nonparametric_normal_test_result <- renderTable({
      data.frame(
        Statistic = ks_test$statistic,
        p_value = ks_test$p.value
      )
    })
  })
  
  # Server function for non-parametric correlation test
  observeEvent(input$run_nonparametric_correlation_test_button, {
    variable1 <- data()[[input$nonparametric_correlation_test_variable1]]
    variable2 <- data()[[input$nonparametric_correlation_test_variable2]]
    spearman_test <- cor.test(variable1, variable2, method = "spearman")
    output$nonparametric_correlation_test_result <- renderTable({
      data.frame(
        Estimate = spearman_test$estimate,
        p_value = spearman_test$p.value
      )
    })
  })
  
  # Server function for non-parametric independence test
  observeEvent(input$run_nonparametric_independence_test_button, {
    variables <- lapply(input$nonparametric_independence_test_variables, function(var) data()[[var]])
    if (length(variables) >= 2) {
      contingency_table <- table(variables)
      if (length(dim(contingency_table)) != 2) {
        output$nonparametric_independence_test_result <- renderText("Chi-square test requires categorical variables.")
        return(NULL)
      }
      chisq_test <- chisq.test(contingency_table)
      output$nonparametric_independence_test_result <- renderTable({
        data.frame(
          Statistic = chisq_test$statistic,
          p_value = chisq_test$p.value
        )
      })
    } else {
      output$nonparametric_independence_test_result <- renderText("Please select at least two variables for the independence test.")
    }
  })
  #tables
  output$table3 <- renderReactable({
    statistical_tests <- data.frame(
      `What is it used for?` = c(
        "Observed Mean vs. Theoretical",
        "Compare two independent means",
        "Compare multiple independent means",
        "Compare two dependent means observed",
        "Compare multiple dependent means observed",
        "Test association between two qualitative variables",
        "Test association between two quantitative variables"
      ),
      `Parametric Test` = c(
        "Jarque-Bera Test",
        "Student's t-test for independent samples",
        "ANOVA",
        "Student's t-test for paired samples",
        "ANOVA with repeated measures / mixed models",
        "Chi-square Test on contingency table",
        "Pearson Correlation Test"
      ),
      `Non-parametric Test` = c(
        "Kolmogorov-Smirnov Test, Shapiro-Wilk Test",
        "Mann-Whitney U Test",
        "Kruskal-Wallis Test",
        "Wilcoxon Signed-Rank Test",
        "Friedman Test",
        "Fisher's Exact Test",
        "Spearman Correlation Test"
      ),
      `Normality` = c(
        "✓",
        "",
        "",
        "",
        "",
        "",
        "✓"
      ),
      `Correlation` = c(
        "",
        "",
        "",
        "",
        "",
        "",
        "✓"
      ),
      `Independence` = c(
        "",
        "✓",
        "✓",
        "✓",
        "✓",
        "✓",
        ""
      )
    )
        reactable(statistical_tests)

  })
  
  
  
  
  
  
  
  
  
  
  
  # Data output
  output$tbl <- DT::renderDataTable({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    DT::datatable(data,
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print")
                  )
    )
  })
  
  output$data <- renderUI({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- data[, input$linear_regression_variable1]
    y <- data[, input$linear_regression_variable2]
    withMathJax(
      paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
      br(),
      paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
      br(),
      paste0("\\(n =\\) ", length(x))
    )
  })
  
  output$by_hand <- renderUI({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    withMathJax(
      paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
      br(),
      paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
      br(),
      br(),
      paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
    )
  })
  
  output$summary <- renderPrint({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  output$results <- renderUI({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })
  
  output$interpretation <- renderUI({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independence, linearity, homoscedasticity, outliers and normality) are met before interpreting the coefficients.)"),
        br(),
        paste0("For a (hypothetical) value of ", input$linear_regression_variable1, " = 0, the mean of ", input$linear_regression_variable2, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("For an increase of one unit of ", input$linear_regression_variable1, ", ", input$linear_regression_variable2, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independence, linearity, homoscedasticity, outliers and normality) are met before interpreting the coefficients.)"),
        br(),
        paste0("For a (hypothetical) value of ", input$linear_regression_variable1, " = 0, the mean of ", input$linear_regression_variable2, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$linear_regression_variable1, " and ", input$linear_regression_variable2, ".")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independence, linearity, homoscedasticity, outliers and normality) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$linear_regression_variable1, " = 0, the mean of ", input$linear_regression_variable2, " is not significantly different from 0."),
        br(),
        paste0("For an increase of one unit of ", input$linear_regression_variable1, ", ", input$linear_regression_variable2, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independence, linearity, homoscedasticity, outliers and normality) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$linear_regression_variable2, " is not significantly different from 0.")
      )
    }
  })
  
  output$assumptions <- renderPlot({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    par(mfrow = c(2, 2))
    plot(fit, which = c(1:3, 5))
  })
  
  output$plot1 <- renderPlotly({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    x <- as.numeric(data[, input$linear_regression_variable1])
    y <- as.numeric(data[, input$linear_regression_variable2])
    fit <- lm(y ~ x)
    p <- ggplot(data, aes_string(x = input$linear_regression_variable1, y = input$linear_regression_variable2)) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab(input$linear_regression_variable2) +
      xlab(input$linear_regression_variable1) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, HTML = "html"
      ))
    },
    
    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format, HTML = html_document()
      ))
      file.rename(out, file)
    }
  )
  # Normality test using Anderson-Darling test
  output$normal_test_result <- renderText({
    req(input$run_normal_test_button)
    if (!is.null(data())) {
      variable <- input$normal_test_variable
      if (!is.null(variable) && variable != "") {
        result <- ad.test(data()[[variable]])
        if (result$p.value <= 0.05) {
          paste("Normality Test Result for", variable, ": Reject null hypothesis (p-value =", result$p.value, ")")
        } else {
          paste("Normality Test Result for", variable, ": Fail to reject null hypothesis (p-value =", result$p.value, ")")
        }
      } else {
        "Please select a variable."
      }
    } else {
      "Please upload a dataset."
    }
  })
  
  #fisher test
  output$fisher_test_result <- renderPrint({
    req(input$run_fisher_test_button)
    if (!is.null(data())) {
      var1 <- input$fisher_test_variable1
      var2 <- input$fisher_test_variable2
      if (!is.null(var1) && var1 != "" && !is.null(var2) && var2 != "") {
        contingency_table <- table(data()[[var1]], data()[[var2]])
        result <- fisher.test(contingency_table, simulate.p.value = TRUE)
        paste("Fisher's Exact Test Result between", var1, "and", var2, ": p-value =", result$p.value)
      } else {
        "Please select both variables."
      }
    } else {
      "Please upload a dataset."
    }
  }) 
 #anova
  output$anova_test_result <- renderPrint({
    req(input$run_anova_button)
    if (!is.null(data())) {
      anova_variables <- input$anova_variables
      if (length(anova_variables) >= 2) {
        result <- tryCatch(
          perform_anova_test(data(), anova_variables),
          error = function(e) {
            "Error: Unable to perform ANOVA test. Please check your input."
          }
        )
        if (!is.null(result)) {
          paste("ANOVA Test Result:", result)
        } else {
          "Invalid formula."
        }
      } else {
        "Please select at least two variables."
      }
    } else {
      "Please upload a dataset."
    }
  })
  
  perform_anova_test <- function(data, variables) {
    # Construct formula
    formula_str <- as.formula(paste(variables[1], "~", paste(variables[-1], collapse = "+")))
    
    # Perform ANOVA test
    aov_result <- aov(formula_str, data = data)
    summary(aov_result)
  }
  #ks
  # Kolmogorov-Smirnov test
  output$ks_test_result <- renderPrint({
    req(input$run_ks_test_button)
    req(!is.null(data()))
    
    variable <- input$ks_test_variable
    
    if (is.null(variable) || variable == "") {
      return("Please select a variable.")
    }
    
    result <- tryCatch(
      ks.test(data()[[variable]], "pnorm"),
      error = function(e) {
        return("Error: Unable to perform Kolmogorov-Smirnov test. Please check your input.")
      }
    )
    
    if (!is.null(result) && inherits(result, "htest")) {
      p_value <- result$p.value
      output_text <- paste("P-value for Normal Distribution for", variable, ":", format.pval(p_value, digits = 4))
      return(output_text)
    } else {
      return("Invalid formula.")
    }
  })
  #shapiro
  # Shapiro-Wilk test
  output$shapiro_test_result <- renderPrint({
    req(input$run_shapiro_test_button)
    if (!is.null(data())) {
      variable <- input$shapiro_test_variable
      if (!is.null(variable) && variable != "") {
        result <- tryCatch(
          shapiro.test(data()[[variable]]),
          error = function(e) {
            return("Error: Unable to perform Shapiro-Wilk test. Please check your input.")
          }
        )
        if (!inherits(result, "try-error")) {
          if (result$p.value <= 0.05) {
            paste("Shapiro-Wilk Normality Test Result for", variable, ": Reject null hypothesis (p-value =", result$p.value, ")")
          } else {
            paste("Shapiro-Wilk Normality Test Result for", variable, ": Fail to reject null hypothesis (p-value =", result$p.value, ")")
          }
        } else {
          return("Invalid formula.")
        }
      } else {
        return("Please select a variable.")
      }
    } else {
      return("Please upload a dataset.")
    }
  })
  #statinf
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  t.test2 <- function(x, V, m0 = 0, alpha = 0.05, alternative = "two.sided") {
    M <- mean(x)
    n <- length(x)
    sigma <- sqrt(V)
    S <- sqrt(V / n)
    statistic <- (M - m0) / S
    p <- if (alternative == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M - S * qnorm(1 - alpha / 2))
    UCL <- (M + S * qnorm(1 - alpha / 2))
    value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
    return(value)
  }
  t.test3 <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, alternative = "two.sided") {
    M1 <- mean(x)
    M2 <- mean(y)
    n1 <- length(x)
    n2 <- length(y)
    sigma1 <- sqrt(V1)
    sigma2 <- sqrt(V2)
    S <- sqrt((V1 / n1) + (V2 / n2))
    statistic <- (M1 - M2 - m0) / S
    p <- if (alternative == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
    UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
    value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
    # print(sprintf("P-value = %g",p))
    # print(sprintf("Lower %.2f%% Confidence Limit = %g",
    #               alpha, LCL))
    # print(sprintf("Upper %.2f%% Confidence Limit = %g",
    #               alpha, UCL))
    return(value)
  }
  prop.z.test <- function(x, n, p0 = 0.5, conf.level = 0.95, alternative = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat <- x / n
    qhat <- 1 - phat
    SE.phat <- sqrt((phat * qhat) / n)
    ts.z <- (phat - p0) / SE.phat
    p.val <- if (alternative == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    cint <- phat + c(
      -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
      ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
    )
    return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  prop.z.test2 <- function(x1, x2, n1, n2, p0 = 0, pooled.stderr = TRUE, conf.level = 0.95, alternative = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat1 <- x1 / n1
    qhat1 <- 1 - phat1
    phat2 <- x2 / n2
    qhat2 <- 1 - phat2
    pooled.phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)
    pooled.qhat <- 1 - pooled.phat
    if (pooled.stderr == FALSE) {
      SE.phat <- sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
    } else {
      SE.phat <- sqrt(pooled.phat * pooled.qhat * (1 / n1 + 1 / n2))
    }
    ts.z <- (phat1 - phat2 - p0) / SE.phat
    p.val <- if (alternative == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    cint <- (phat1 - phat2) + c(
      -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
      ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
    )
    return(list(x1 = x1, x2 = x2, n1 = n1, n2 = n2, estimate1 = phat1, estimate2 = phat2, null.value = p0, stderr = SE.phat, pooled.phat = pooled.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  prop.z.test3 <- function(x, n, p0 = 0.5, conf.level = 0.95, alternative = "two.sided") {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat <- x / n
    qhat <- 1 - phat
    SE.phat <- sqrt((p0 * (1-p0)) / n)
    ts.z <- (phat - p0) / SE.phat
    p.val <- if (alternative == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    return(list(x = x, n = n, estimate = phat, null.value = p0, stderr = SE.phat, statistic = ts.z, p.value = p.val))
  }
  
  output$results_onemean <- renderUI({
    dat <- extract(input$sample_onemean)
    if (anyNA(dat) | length(dat) < 2) {
      "Invalid input or not enough observations"
    } else if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      test_confint <- t.test(x = dat, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat)), 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "one mean" & input$popsd_onemean == TRUE) {
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(dat), 3)),
        br(),
        paste0("\\(\\sigma =\\) ", round(sqrt(input$sigma2_onemean), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu = \\bar{x} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", input$h0, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{\\sigma / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat)), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), input$h0, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_twomeanspaired <- renderUI({
    dat1 <- extract(input$sample1_twomeanspaired)
    dat2 <- extract(input$sample2_twomeanspaired)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (length(dat1) != length(dat2)) {
      "Number of observations must be equal in the two samples"
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == FALSE) {
      test_confint <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = TRUE)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(s^2_D =\\) ", round(var(dat2 - dat1), 3)),
        br(),
        paste0("\\(s_D =\\) ", round(sd(dat2 - dat1), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr * sqrt(length(dat1)), 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", test$null.value, " and \\(H_1 : \\mu_D \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{D} - \\mu_0}{s_D / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean of the difference is equal to ", "we do not reject the null hypothesis that the true mean of the difference is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == TRUE) {
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
        br(),
        paste0("Number of pairs \\(n =\\) ", length(dat1)),
        br(),
        paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
        br(),
        paste0("\\(\\sigma^2_D =\\) ", round(input$sigma2_twomeanspaired, 3)),
        br(),
        paste0("\\(\\sigma_D =\\) ", round(sqrt(input$sigma2_twomeanspaired), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_D = \\bar{D} \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} = \\) ",
          round(test$mean, 3), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$sigma, 3), " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h0, " and \\(H_1 : \\mu_D \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{D} - \\mu_0}{\\sigma_D / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$sigma / sqrt(length(dat1)), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true mean of the difference is equal to ", "we do not reject the null hypothesis that the true mean of the difference is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_twomeans <- renderUI({
    dat1 <- extract(input$sample1_twomeans)
    dat2 <- extract(input$sample2_twomeans)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      s_p <- sqrt(((length(dat1) - 1) * var(dat1) + (length(dat2) - 1) * var(dat2)) / test_confint$parameter)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", round(s_p, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / length(dat1) + 1 / length(dat2)), 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / (", round(s_p, 3), " * ", round(sqrt((1 / length(dat1)) + (1 / length(dat2))), 3), ") \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      test_confint <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}} \\)"),
        br(),
        paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(test$parameter, 3)),
        br(),
        br(),
        paste0(
          "\\( \\Rightarrow \\)", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", ifelse(input$alternative == "greater", " \\( t_{\\alpha, \\nu} = t(\\)", " \\( -t_{\\alpha, \\nu} = -t(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), ", ", round(test$parameter, 3), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two means" & input$popsd_twomeans == TRUE) {
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
        br(),
        paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
        br(),
        paste0("\\(\\sigma^2_1 =\\) ", round(input$sigma21_twomeans, 3)),
        br(),
        paste0("\\(\\sigma^2_2 =\\) ", round(input$sigma22_twomeans, 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}} = \\) ",
          round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test$S, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h0, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
          "(", round(test$mean1, 3), ifelse(test$mean2 >= 0, paste0(" - ", round(test$mean2, 3)), paste0(" + ", round(abs(test$mean2), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", round(test$S, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_oneprop <- renderUI({
    if (input$inference == "one proportion" & input$propx_oneprop == "prop_true") {
      test <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\hat{p} =\\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\hat{q} = 1 - \\hat{p} =\\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\hat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\hat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\hat{p} \\geq 5\\) and \\( n(1-\\hat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} = \\) ",
          "(", round(test2$estimate, 3), ifelse(test2$null.value >= 0, paste0(" - ", test2$null.value), paste0(" + ", abs(test2$null.value))), ") / ", round(test2$stderr, 3), " \\( = \\) ",
          ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test2$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test2$p.value < input$alpha, "we reject the null hypothesis that the true proportion is ", "we do not reject the null hypothesis that the true proportion is "), test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "one proportion" & input$propx_oneprop == "prop_false") {
      test <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test2 <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      test_confint <- prop.z.test(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided")
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n =\\) ", round(test$n, 3)),
        br(),
        paste0("\\(\\hat{p} = \\dfrac{x}{n} = \\) ", test$x, " \\( / \\) ", test$n, " \\( = \\) ", round(test$estimate, 3)),
        br(),
        paste0("\\(\\hat{q} = 1 - \\hat{p} = \\) ", round(1 - test$estimate, 3)),
        br(),
        helpText(paste0("\\( n\\hat{p} = \\) ", round(test$n * test$estimate, 3), " and \\( n(1-\\hat{p}) = \\) ", round(test$n * (1 - test$estimate), 3))),
        helpText(paste0("Assumptions \\( n\\hat{p} \\geq 5\\) and \\( n(1-\\hat{p}) \\geq 5\\)", ifelse(test$n * test$estimate >= 5 & test$n * (1 - test$estimate) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\) ",
          round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} = \\) ",
          "(", round(test2$estimate, 3), ifelse(test2$null.value >= 0, paste0(" - ", test2$null.value), paste0(" + ", abs(test2$null.value))), ") / ", round(test2$stderr, 3), " \\( = \\) ",
          ifelse(test2$null.value >= 0 & test2$null.value <= 1, round(test2$statistic, 3), "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test2$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test2$p.value < input$alpha, "we reject the null hypothesis that the true proportion is ", "we do not reject the null hypothesis that the true proportion is "), test2$null.value, " \\((p\\)-value ", ifelse(test2$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test2$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_twoprop <- renderUI({
    if (input$inference == "two proportions" & input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == FALSE) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == FALSE) {
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == TRUE) {
      test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\hat{p} = \\dfrac{n_1 \\hat{p}_1 + n_2 + \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else if (input$inference == "two proportions" & input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == TRUE) {
      test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      test_confint <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = "two.sided", pooled.stderr = FALSE)
      withMathJax(
        paste("Your data:"),
        br(),
        paste0("\\(n_1 =\\) ", round(test$n1, 3)),
        br(),
        paste0("\\(n_2 =\\) ", round(test$n2, 3)),
        br(),
        paste0("\\(\\hat{p}_1 = \\dfrac{x_1}{n_1} = \\) ", test$x1, " \\( / \\) ", test$n1, " \\( = \\) ", round(test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 = \\dfrac{x_2}{n_2} = \\) ", test$x2, " \\( / \\) ", test$n2, " \\( = \\) ", round(test$estimate2, 3)),
        br(),
        paste0("\\(\\hat{q}_1 = 1 - \\hat{p}_1 =\\) ", round(1 - test$estimate1, 3)),
        br(),
        paste0("\\(\\hat{q}_2 = 1 - \\hat{p}_2 =\\) ", round(1 - test$estimate2, 3)),
        br(),
        helpText(paste0("\\( n_1\\hat{p}_1 = \\) ", round(test$n1 * test$estimate1, 3), " and \\( n_1(1-\\hat{p}_1) = \\) ", round(test$n1 * (1 - test$estimate1), 3))),
        helpText(paste0("\\( n_2\\hat{p}_2 = \\) ", round(test$n2 * test$estimate2, 3), " and \\( n_2(1-\\hat{p}_2) = \\) ", round(test$n2 * (1 - test$estimate2), 3))),
        helpText(paste0("Assumptions \\( n_1\\hat{p}_1 \\geq 5\\), \\( n_1(1-\\hat{p}_1) \\geq 5\\), \\( n_2\\hat{p}_2 \\geq 5\\) and \\( n_2(1-\\hat{p}_2) \\geq 5\\)", ifelse(test$n1 * test$estimate1 >= 5 & test$n1 * (1 - test$estimate1) >= 5 & test$n2 * test$estimate2 >= 5 & test$n2 * (1 - test$estimate2) >= 5, " are met.", " are not met."))),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(test_confint$estimate1, 3), ifelse(test_confint$estimate2 >= 0, paste0(" - ", round(test_confint$estimate2, 3)), paste0(" + ", round(abs(test_confint$estimate2), 3))), "  \\( \\pm \\) ", "\\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), " * ", round(test_confint$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", test$null.value, " and \\(H_1 : p_1 - p_2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("where ", "\\( \\hat{p} = \\dfrac{n_1 \\hat{p}_1 + n_2 + \\hat{p}_2}{n_1 + n_2} = \\) ", "(", test$n1, " * ", round(test$estimate1, 3), " + ", test$n2, " * ", round(test$estimate2, 3), ") / (", test$n1, " + ", test$n2, ") = ", round(test$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
          "(", round(test$estimate1, 3), ifelse(test$estimate2 >= 0, paste0(" - ", round(test$estimate2, 3)), paste0(" + ", round(abs(test$estimate2), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(test$stderr, 3), " \\( = \\) ",
          ifelse(test$null.value >= -1 & test$null.value <= 1, round(test$statistic, 3), "Error: \\( p_1 - p_2 \\) must be \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative == "two.sided", "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
          ifelse(input$alternative == "two.sided", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), round(qnorm(input$alpha, lower.tail = FALSE), 3))
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true difference in proportions is ", "we do not reject the null hypothesis that the true difference in proportions is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_onevar <- renderUI({
    dat <- extract(input$sample_onevar)
    if (anyNA(dat) | length(dat) < 2) {
      "Invalid input or not enough observations"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_0 \\) must be > 0")
      )
    } else if (input$inference == "one variance") {
      test_confint <- varTest(x = dat, sigma.squared = input$h0, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      withMathJax(
        paste(c("Your data:", paste(dat, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n =\\) ", length(dat)),
        br(),
        paste0("\\(s^2 =\\) ", round(var(dat), 3)),
        br(),
        paste0("\\(s =\\) ", round(sd(dat), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\(\\sigma^2 = \\Bigg[ \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}} ; \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg] = \\) ",
          "[(", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), ") ; (", round((length(dat) - 1) * test$estimate, 3), " / ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = TRUE), 3), ")] = ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\sigma^2 = \\) ", test$null.value, " and \\(H_1 : \\sigma^2 \\) ", ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(\\chi^2_{obs} = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
          "[(", length(dat), " - 1) * ", round(test$estimate, 3), "] / ", test$null.value, " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (input$alternative == "two.sided") {
          withMathJax(
            paste0("3. Critical values : \\( \\chi^2_{1-\\alpha/2, n - 1} \\) and \\( \\chi^2_{\\alpha/2, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha / 2, ", ", test$parameters, ") and \\( \\chi^2 \\)(", input$alpha / 2, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3), " and ", round(qchisq(input$alpha / 2, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else if (input$alternative == "greater") {
          withMathJax(
            paste0("3. Critical value : \\( \\chi^2_{\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Critical value : \\( \\chi^2_{1-\\alpha, n - 1} =\\) "),
            paste0("\\( \\chi^2 \\)(", 1 - input$alpha, ", ", test$parameters, ") = "),
            paste0(round(qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE), 3))
          )
        },
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true variance is equal to ", "we do not reject the null hypothesis that the true variance is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$results_twovar <- renderUI({
    dat1 <- extract(input$sample1_twovar)
    dat2 <- extract(input$sample2_twovar)
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (input$h0 <= 0) {
      withMathJax(
        sprintf("\\( \\sigma^2_1 - \\sigma^2_2 \\) must be > 0")
      )
    } else if (input$inference == "two variances") {
      test_confint <- var.test(x = dat1, y = dat2, ratio = 1, alternative = "two.sided", conf.level = 1 - input$alpha)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
      withMathJax(
        paste("Your data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        paste0("\\(n_1 =\\) ", length(dat1)),
        br(),
        paste0("\\(n_2 =\\) ", length(dat2)),
        br(),
        paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
        br(),
        paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
        br(),
        paste0("\\(s_1 =\\) ", round(sd(dat1), 3)),
        br(),
        paste0("\\(s_2 =\\) ", round(sd(dat2), 3)),
        br(),
        br(),
        tags$b("Confidence interval"),
        br(),
        paste0(
          (1 - input$alpha) * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = \\Bigg[ \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} ; \\dfrac{s^2_1}{s^2_2}F_{\\alpha/2, n_1 - 1, n_2-1} \\Bigg] = \\) ",
          "\\( \\big[ \\)", round(test_confint$estimate, 3), " * (1 / ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "); ", round(test_confint$estimate, 3), " * ", round(qf(input$alpha / 2, df1 = test_confint$parameter[1], df2 = test_confint$parameter[2], lower.tail = FALSE), 3), "\\( \\big] = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        if (test$alternative == "two.sided") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
          )
        } else if (test$alternative == "greater") {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
          )
        } else {
          withMathJax(
            paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
          )
        },
        br(),
        paste0(
          "2. Test statistic : \\(F_{obs} = \\dfrac{s^2_1}{s^2_2} = \\) ",
          "[", round(var(dat1), 3), " / ", round(var(dat2), 3), "]", " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        if (test$alternative == "two.sided") {
          withMathJax(
            paste0("3. Critical values : \\( F_{1-\\alpha/2, n_1 - 1, n_2-1} \\) and \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( \\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}} \\) and \\( F_{\\alpha/2, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( 1/F \\)(", input$alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") and \\( F \\)(", input$alpha / 2, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3), " and ", round(qf(input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
          )
        } else if (test$alternative == "greater") {
          withMathJax(
            paste0("3. Critical value : \\( F_{\\alpha, n_1 - 1, n_2-1} =\\) "),
            paste0("\\( F \\)(", input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE), 3))
          )
        } else {
          withMathJax(
            paste0("3. Critical values : \\( F_{1-\\alpha, n_1 - 1, n_2-1} = \\) "),
            paste0("\\( \\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}} = \\) "),
            paste0("\\( 1/F \\)(", input$alpha, ", ", test$parameter[1], ", ", test$parameter[2], ") = "),
            paste0(round(qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE), 3))
          )
        },
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", ifelse(test$p.value < input$alpha, "we reject the null hypothesis that the true ratio of variances is equal to ", "we do not reject the null hypothesis that the true ratio of variances is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
      )
    } else {
      print("loading...")
    }
  })
  
  output$plot <- renderPlot({
    if (input$inference == "one mean" & input$popsd_onemean == FALSE) {
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one mean" & input$popsd_onemean == TRUE) {
      dat <- extract(input$sample_onemean)
      test <- t.test2(x = dat, V = input$sigma2_onemean, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = TRUE)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test(x = dat1, y = dat2, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = FALSE, var.equal = FALSE)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means" & input$popsd_twomeans == TRUE) {
      dat1 <- extract(input$sample1_twomeans)
      dat2 <- extract(input$sample2_twomeans)
      test <- t.test3(x = dat1, y = dat2, V1 = input$sigma21_twomeans, V2 = input$sigma22_twomeans, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == FALSE) {
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat2, y = dat1, mu = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha, paired = TRUE)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$alpha / 2, df = test$parameter) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x < qt(input$alpha, df = test$parameter, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dt(x, df = test$parameter)
          y[x > qt(input$alpha, df = test$parameter, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = test$parameter)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two means (paired samples)" & input$popsd_twomeanspaired == TRUE) {
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test2(x = dat2 - dat1, V = input$sigma2_twomeanspaired, m0 = input$h0, alpha = input$alpha, alternative = input$alternative)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one proportion") {
      if (input$propx_oneprop == "prop_true") {
        test <- prop.z.test3(x = input$n_oneprop * input$p_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      } else {
        test <- prop.z.test3(x = input$x_oneprop, n = input$n_oneprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two proportions") {
      if (input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == FALSE) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == FALSE) {
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = FALSE)
      } else if (input$propx_twoprop == "prop_true" & input$pooledstderr_twoprop == TRUE) {
        test <- prop.z.test2(x1 = input$n1_twoprop * input$p1_twoprop, x2 = input$n2_twoprop * input$p2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      } else if (input$propx_twoprop == "prop_false" & input$pooledstderr_twoprop == TRUE) {
        test <- prop.z.test2(x1 = input$x1_twoprop, x2 = input$x2_twoprop, n1 = input$n1_twoprop, n2 = input$n2_twoprop, p0 = input$h0, conf.level = 1 - input$alpha, alternative = input$alternative, pooled.stderr = TRUE)
      }
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) & x > qnorm(input$alpha / 2, mean = 0, sd = 1) ] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x < qnorm(input$alpha, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dnorm(x, mean = 0, sd = 1)
          y[x > qnorm(input$alpha, mean = 0, sd = 1, lower.tail = TRUE) ] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "one variance") {
      dat <- extract(input$sample_onevar)
      test <- varTest(x = dat, sigma.squared = input$h0, alternative = input$alternative, conf.level = 1 - input$alpha)
      if (input$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = FALSE) & x < qchisq(1 - input$alpha / 2, df = test$parameters, lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (input$alternative == "greater") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x < qchisq(input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (input$alternative == "less") {
        funcShaded <- function(x) {
          y <- dchisq(x, df = test$parameters)
          y[x > qchisq(1 - input$alpha, df = test$parameters, lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qchisq(0.999, df = test$parameters, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = test$parameters)) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.025), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Chi-square distribution (df = ", test$parameters, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else if (input$inference == "two variances") {
      dat1 <- extract(input$sample1_twovar)
      dat2 <- extract(input$sample2_twovar)
      test <- var.test(x = dat1, y = dat2, ratio = 1, alternative = input$alternative_twovar, conf.level = 1 - input$alpha)
      if (test$alternative == "two.sided") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE) & x < qf(1 - input$alpha / 2, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE)] <- NA
          return(y)
        }
      } else if (test$alternative == "greater") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x < qf(input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      } else if (test$alternative == "less") {
        funcShaded <- function(x) {
          y <- df(x, df1 = test$parameter[1], df2 = test$parameter[2])
          y[x > qf(1 - input$alpha, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = FALSE)] <- NA
          return(y)
        }
      }
      p <- ggplot(data.frame(x = c(0, qf(0.99, df1 = test$parameter[1], df2 = test$parameter[2], lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = df, args = list(df1 = test$parameter[1], df2 = test$parameter[2])) +
        stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = test$statistic, color = "steelblue") +
        geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("F distribution F(", test$parameter[1], ", ", test$parameter[2], ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    } else {
      print("loading...")
    }
  })
  
  app_dt <- reactive({
    if (is.null(input$your_dt)) {
      out <- read_img_as_array(input$demo_dt)
    } else {
      datapath <- input$your_dt$datapath
      if (tools::file_ext(datapath) == "gz") {
        datapath <- sub("gz$", "nii.gz", datapath)
        file.rename(input$your_dt$datapath, datapath)
      }
      out <- read_img_as_array(datapath)
    }
    return(out)
  })
  
  output$raster_panel <- renderUI({
    if (input$interactive) {
      callModule(raster3d_interactive_Module, "mri_3d", im = app_dt)
      raster3d_interactive_UI("mri_3d")
    } else {
      callModule(raster3d_animation_Module, "mri_3d", im = app_dt)
      raster3d_animation_UI("mri_3d")
    }
  })
  
  output$data_summary <- renderDataTable({
    if (!is.null(data())) {
      datatable(data(), options = list(scrollX = TRUE))
    }
  })
  
  output$missing_values_table <- renderDataTable({
    if (!is.null(data())) {
      missing_values <- sapply(data(), function(x) sum(is.na(x)))
      missing_percentage <- round((missing_values / nrow(data())) * 100, 2)
      missing_data <- data.frame(Column = names(missing_values), Missing_Values = missing_values, Percentage = missing_percentage)
      
      datatable(missing_data, options = list(scrollX = TRUE))
    }
  })
  
  
  # Data cleaning process
  observeEvent(input$clean_data_button, {
    if (!is.null(data())) {
      if (input$delete_rows) {
        data(data()[complete.cases(data()), ])
      }
      
      if (input$fill_missing_values) {
        fill_value <- switch(input$fill_with,
                             "0" = 0,
                             "Mean" = mean(data() %>% select(where(is.numeric)), na.rm = TRUE),
                             "Median" = median(data() %>% select(where(is.numeric)), na.rm = TRUE),
                             "Max" = max(data() %>% select(where(is.numeric)), na.rm = TRUE)
        )
        
        data(data() %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), fill_value, .))))
      }
      
      # Inform the user that cleaning is done
      showModal(modalDialog(
        title = "Data Cleaning",
        "Data cleaning process has been completed.",
        footer = NULL,
        easyClose = TRUE
      ))
    }
  })
#Overview table stats
  
  output$overview_statistics_table <- renderDataTable({
    if (!is.null(data())) {
      selected_vars <- input$overview_variables
      if (length(selected_vars) > 0) {
        summary_stats <- sapply(data()[selected_vars], function(x) {
          c(min = min(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE),
            mean = mean(x, na.rm = TRUE),
            median = median(x, na.rm = TRUE),
            q1 = quantile(x, probs = 0.25, na.rm = TRUE),
            q3 = quantile(x, probs = 0.75, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE),
            var = var(x, na.rm = TRUE))
        })
        summary_stats_df <- as.data.frame(t(summary_stats))
        colnames(summary_stats_df) <- c("Min", "Max", "Mean", "Median", "Q1", "Q3", "SD", "Var")
        return(summary_stats_df)
      } else {
        return("Please select at least one variable.")
      }
    }
  })
  
 
  # Barplot
  output$barplot <- renderPlot({
    if (!is.null(data())) {
      selected_vars <- input$barplot_variables
      show_counts <- input$show_counts
      color_code <- input$color_code_barplot
      
      if (!is.null(color_code) && nchar(color_code) == 7 && substr(color_code, 1, 1) == "#") {
        palette <- rep(color_code, length(selected_vars))
      } else {
        palette <- NULL
      }
      
      if (length(selected_vars) > 0) {
        if (show_counts) {
          barplot(colSums(data()[selected_vars]), col = palette)
        } else {
          barplot(colMeans(data()[selected_vars]), col = palette)
        }
      } else {
        plot(0, type = "n", xlab = "", ylab = "", main = "No variables selected")
      }
    }
  })
  
  # Histogram
  output$histogram <- renderPlot({
    if (!is.null(data())) {
      selected_vars <- input$histogram_variables
      bins <- input$histogram_bins
      color_code <- input$color_code_histogram
      
      if (length(selected_vars) > 0) {
        if (nchar(color_code) == 7 && substr(color_code, 1, 1) == "#") {
          hist_data <- lapply(selected_vars, function(var) {
            hist(data()[[var]], breaks = bins, xlab = "Value", main = paste("Histogram of", var), col = color_code)
          })
        } else {
          hist_data <- lapply(selected_vars, function(var) {
            hist(data()[[var]], breaks = bins, xlab = "Value", main = paste("Histogram of", var))
          })
        }
      } else {
        plot(0, type = "n", xlab = "", ylab = "", main = "No variables selected")
      }
    }
  })
  
  # Pie Chart
  output$pie_chart <- renderPlotly({
    if (!is.null(data())) {
      selected_var <- input$piechart_variables
      if (!is.null(selected_var) && length(selected_var) > 0) {
        counts <- table(data()[[selected_var]])
        df <- as.data.frame(counts)
        colnames(df) <- c("Category", "Count")
        total <- sum(df$Count)
        df$percent <- df$Count / total * 100
        df$label <- paste0(df$Category, ": ", round(df$percent, 2), "%")
        
        p <- plot_ly(df, labels = ~label, values = ~Count, type = 'pie') %>%
          layout(title = "Pie Chart")
        
        p
      } else {
        plot_ly(data.frame(x = 1, y = 1), type = "scatter", mode = "text",
                text = "No data available", hoverinfo = "none") %>%
          layout(showlegend = FALSE, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    }
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    if (!is.null(data())) {
      selected_vars <- input$boxplot_variables
      
      if (length(selected_vars) > 0) {
        if (input$color_code_boxplot) {
          boxplot_data <- lapply(selected_vars, function(var) {
            data.frame(variable = var, value = data()[[var]])
          })
          
          boxplot_data <- do.call(rbind, boxplot_data)
          
          p <- ggplot(boxplot_data, aes(x = variable, y = value)) +
            geom_boxplot(fill = input$color_code_boxplot_input) +
            labs(title = "Boxplot")
          
          ggplotly(p)
        } else {
          # No custom color provided
          boxplot_data <- lapply(selected_vars, function(var) {
            data.frame(variable = var, value = data()[[var]])
          })
          
          boxplot_data <- do.call(rbind, boxplot_data)
          
          p <- ggplot(boxplot_data, aes(x = variable, y = value)) +
            geom_boxplot() +
            labs(title = "Boxplot")
          
          ggplotly(p)
        }
      } else {
        plot_ly(data.frame(x = 1, y = 1), type = "scatter", mode = "text",
                text = "No variables selected", hoverinfo = "none") %>%
          layout(showlegend = FALSE, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    }
  })
  
  # Density Plot
  output$density_plot <- renderPlot({
    if (!is.null(data())) {
      selected_vars <- input$densityplot_variables
      
      if (length(selected_vars) > 0) {
        # Create a combined data frame for density plot
        density_data <- lapply(selected_vars, function(var) {
          data.frame(variable = rep(var, length(data()[[var]])), value = data()[[var]])
        })
        density_data <- do.call(rbind, density_data)
        
        # Plot density
        ggplot(density_data, aes(x = value, fill = variable)) +
          geom_density(alpha = 0.5) +
          labs(title = "Density Plot") +
          theme_minimal()
      } else {
        plot(0, type = "n", xlab = "", ylab = "", main = "No variables selected")
      }
    }
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    if (!is.null(data())) {
      selected_vars <- input$scatterplot_variables
      
      if (length(selected_vars) == 2) {
        plot_ly(data(), x = ~get(selected_vars[1]), y = ~get(selected_vars[2]), mode = "markers")
      } else {
        plot_ly(data.frame(x = 1, y = 1), type = "scatter", mode = "text",
                text = "Please select two variables", hoverinfo = "none") %>%
          layout(showlegend = FALSE, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    }
  })
  
  
  
  # Human Anatomy Plot
  output$Human_Anatomy_Plot <- renderUI({
    
    allSpecies <- c("human_male", "human_female", "mouse_male", "mouse_female", "cell", names(other_key))
    selectInput("SpeciesInput", "Species",
                choices=allSpecies,
                selected=allSpecies[1])
  })
  
  
  output$fill <- renderUI({
    if (is.null(anat_key())) {
      return(NULL)
    }
    selectInput("fillInput", "Fill based on colour or value. Both colour and value can be changed in the table",
                c("colour", "value"),
                selected = c("value"))
  })
  
  
  
  anat_key <- reactive({
    if (is.null(input$SpeciesInput)) {
      return(NULL)
    }
    selectedSpecies <- input$SpeciesInput
    if (selectedSpecies == "human_male" ) {
      hgMale_key
    } else if (selectedSpecies =="human_female" ) {
      hgFemale_key
    } else if (selectedSpecies=="mouse_male" ) {
      mmMale_key
    } else if (selectedSpecies=="mouse_female" ) {
      mmFemale_key
    } else if (selectedSpecies =="cell" ) {
      cell_key[["cell"]]
    } else {
      other_key[[selectedSpecies]]
    }
    
  })
  
  output$Organs <- renderUI({
    if (is.null(anat_key())) {
      return(NULL)
    }
    organs <- anat_key()$organ
    pickerInput("OrgansInput","Organs - Select before changing value", choices = organs, selected = organs, options = list(`actions-box` = TRUE),multiple = T)
  })
  
  output$valueColour <- renderUI({
    colourOptions <- c('viridis', 'magma', 'inferno', 'plasma', 'cividis', rownames(brewer.pal.info[brewer.pal.info$category != 'qual',]))
    #"magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
    selectInput("colourValue", "Value colour",
                choices=colourOptions,
                selected='viridis')
    
  })
  
  
  organism <- reactive({
    if (is.null(anat_key())) {
      return(NULL)
    }
    selectedSpecies <- input$SpeciesInput
    if (selectedSpecies == "human_male" | selectedSpecies == "human_female"  ) {
      "human"
    } else if (selectedSpecies=="mouse_male" | selectedSpecies=="mouse_female"  ) {
      "mouse"
    } else if (selectedSpecies =="cell" ) {
      "cell"
    } else {
      selectedSpecies
    }
    
  })
  
  
  
  sex <- reactive({
    if (is.null(anat_key())) {
      return(NULL)
    }
    selectedSpecies <- input$SpeciesInput
    if (selectedSpecies == "human_male" | selectedSpecies == "mouse_male"  ) {
      "male"
    } else if (selectedSpecies=="mouse_female" | selectedSpecies=="human_female"  ) {
      "female"
    } else {
      "female"
    }
    
  })
  
  Reactive_key <- reactiveValues(data = NULL)
  Reactive_key$data <-reactive({ 
    if (is.null(anat_key())) {
      return(NULL)
    }
    anat_key()})

  
  output$mytable2 <- renderRHandsontable({
    if (is.null(anat_key())) {
      return(NULL)
    }
    #print(class())
    
    
    organTable <- Reactive_key$data()
    organTable <- organTable[organTable$organ %in% input$OrgansInput,]
    
    rhandsontable(organTable)
    
  })
  output$medical_comparison_plot <- renderPlot({
    # Create data based on the selected medical issue
    if (input$medical_issue == "Normal") {
      compareGroups <- data.frame(
        organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"),
        value = c(10, 5, 1, 8, 2, 5, 5),
        type = rep('Normal', 7),
        stringsAsFactors = FALSE
      )
    } else if (input$medical_issue == "Cancer") {
      compareGroups <- data.frame(
        organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"),
        value = c(5, 5, 10, 8, 2, 5, 5),
        type = rep('Cancer', 7),
        stringsAsFactors = FALSE
      )
    } else if (input$medical_issue == "Alzheimer") {
      compareGroups <- data.frame(
        organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon", "hippocampus"),
        value = c(10, 5, 1, 8, 2, 5, 5, 10),
        type = rep('Alzheimer', 8),
        stringsAsFactors = FALSE
      )
    } else if (input$medical_issue == "Parkinson") {
      compareGroups <- data.frame(
        organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"),
        value = c(5, 5, 10, 10, 2, 5, 5), # Brain value changed to maximum
        type = rep('Parkinson', 7),
        stringsAsFactors = FALSE
      )
    }
    
    g <- gganatogram(
      data = compareGroups,
      fillOutline = '#a6bddb',
      organism = 'human',
      sex = 'male',
      fill = "value"
    ) + 
      theme_void() +
      facet_wrap(~type) +
      scale_fill_gradient(low = "white", high = "red")
    
    return(g)
  }, height = 650, width = 400)
  
  
  
  reactive({
    reactiveTemp <- Reactive_key$data()
    plotAnat <- hot_to_r(input$mytable2)
    reactiveTemp$value[match(plotAnat$organ, reactiveTemp$organ)] <- plotAnat$value
    # head( Reactive_key$data()[match(plotAnat$organ, Reactive_key$data()$organ),])
    print(head( reactiveTemp$organ))
    print(class(plotAnat))
    Reactive_key$data <- reactiveTemp
  })
  
  
  output$gganatogram <- renderPlot({
    if (is.null(anat_key()) | is.null(input$mytable2)) {
      return(NULL)
    }
    
    
    plotAnat <- hot_to_r(input$mytable2)
    if (length(input$OrgansInput)<1) {
      p <- gganatogram(fillOutline= input$col, outline=input$showOutline, organism=organism(), sex=sex(), fill=input$fillInput) +theme_void() + coord_fixed() + ggtitle(input$ggtitle) +   theme(plot.title = element_text(hjust = 0.5))
    } else {
      plotOrgans <- plotAnat
      plotOrgans <- plotOrgans[plotOrgans$organ %in% input$OrgansInput, ]
      p <- gganatogram(plotOrgans, outline=input$showOutline, fillOutline= input$col, organism=organism(), sex=sex(), fill=input$fillInput) +theme_void() + coord_fixed() +ggtitle(input$ggtitle) +  theme(plot.title = element_text(hjust = 0.5))
      
    }
    if (input$reverseId ) {
      Palettedirection = 1
    } else {
      Palettedirection = -1
    }
    
    if ( input$fillInput == "value" ) {
      if ( input$colourValue %in% c('viridis', 'magma', 'inferno', 'plasma', 'cividis') ) {
        p <- p + scale_fill_viridis(option = input$colourValue, direction= Palettedirection)
      } else {
        p <- p + scale_fill_distiller(palette = input$colourValue, direction = Palettedirection)
      }
    }
    
    p
  })
  
  output$plot.ui <- renderUI({
    if (is.null(input$height) ) {
      ggheight <-100
    } else {
      ggheight <- input$height
    }
    
    
    plotOutput("gganatogram", height = paste0(ggheight, "cm"))
  })


  # JavaScript to disable other checkboxes once one is selected
  output$boxplot_variables_ui <- renderUI({
    req(data())
    num_vars <- length(names(data()[sapply(data(), is.numeric)]))
    checkboxGroupInput("boxplot_variables", "Select Variable:", choices = names(data()[sapply(data(), is.numeric)]), inline = TRUE)
  })
  
  observeEvent(input$boxplot_variables, {
    req(data())
    req(input$boxplot_variables)
    selected_var <- input$boxplot_variables
    if (length(selected_var) > 0) {
      disabled_vars <- setdiff(names(data()[sapply(data(), is.numeric)]), selected_var)
      for (variable in disabled_vars) {
        session$sendCustomMessage("disableCheckbox", list(variable))
      }
    }
  })
  
  # JavaScript to disable other checkboxes once one is selected for pie chart
  output$piechart_variables_ui <- renderUI({
    req(data())
    num_vars <- length(names(data()[sapply(data(), is.numeric)]))
    checkboxGroupInput("piechart_variables", "Select Variable:", choices = names(data()[sapply(data(), is.numeric)]), inline = TRUE)
  })
  
  observeEvent(input$piechart_variables, {
    req(data())
    req(input$piechart_variables)
    selected_var <- input$piechart_variables
    if (length(selected_var) > 0) {
      disabled_vars <- setdiff(names(data()[sapply(data(), is.numeric)]), selected_var)
      for (variable in disabled_vars) {
        session$sendCustomMessage("disableCheckbox", list(variable))
      }
    }
  })
  
  # JavaScript to disable other checkboxes once one is selected for density plot
  output$densityplot_variables_ui <- renderUI({
    req(data())
    num_vars <- length(names(data()[sapply(data(), is.numeric)]))
    checkboxGroupInput("densityplot_variables", "Select Variable:", choices = names(data()[sapply(data(), is.numeric)]), inline = TRUE)
  })
  
  observeEvent(input$densityplot_variables, {
    req(data())
    req(input$densityplot_variables)
    selected_var <- input$densityplot_variables
    if (length(selected_var) > 0) {
      disabled_vars <- setdiff(names(data()[sapply(data(), is.numeric)]), selected_var)
      for (variable in disabled_vars) {
        session$sendCustomMessage("disableCheckbox", list(variable))
      }
    }
  })
  
 
  output$correlation_heatmap <- renderPlot({
    req(data(), input$correlation_variables, input$correlation_method)
    selected_vars <- input$correlation_variables
    correlation_method <- input$correlation_method
    
    # Calculate correlation matrix based on selected method
    correlation_matrix <- switch(correlation_method,
                                 "Pearson's" = cor(data()[, selected_vars], method = "pearson"),
                                 "Spearman's" = cor(data()[, selected_vars], method = "spearman"),
                                 "Kendall's" = cor(data()[, selected_vars], method = "kendall"),
                                 "Cramer's V" = {
                                   if (length(selected_vars) == 2 && all(sapply(data()[, selected_vars], is.factor))) {
                                     contingency_table <- table(data()[, selected_vars])
                                     chisq_res <- chisq.test(contingency_table)
                                     cramer_v <- sqrt(chisq_res$statistic / (sum(contingency_table) * (min(dim(contingency_table)) - 1)))
                                     matrix(cramer_v, nrow = 1, ncol = 1)
                                   } else {
                                     NULL
                                   }
                                 },
                                 "Point-biserial" = {
                                   if (length(selected_vars) == 2 && is.numeric(data()[[selected_vars[1]]]) && is.factor(data()[[selected_vars[2]]])) {
                                     cor.test(data()[[selected_vars[1]]], as.numeric(data()[[selected_vars[2]]]))
                                   } else {
                                     NULL
                                   }
                                 }
    )
    mat <- matrix(rnorm(100), nrow = 10)
    
    # Define the palette
    pal <- paletteer_c("ggthemes::Classic Red-White-Green Light", 30)
    
    # Render the correlation heatmap
    if (!is.null(correlation_matrix)) {
        pheatmap(correlation_matrix, 
                 cluster_rows = TRUE, 
                 cluster_cols = TRUE,
                 display_numbers = TRUE,
                 fontsize_number = 20,
                 number_color = "black",
                 border_color = "black",
                 color = pal,
                 main = "Correlation Pheatmap")
      
    } else {
      plot(0, type = "n", xlab = "", ylab = "", main = "No correlation matrix available")
    }
  })
  output$table1 <- renderReactable({
    correlation_strength <- data.frame(
      `Correlation coefficient` = c("-0.7 to -1", "-0.5 to -0.7", "-0.3 to -0.5", "0 to -0.3", "0", "0 to 0.3", "0.3 to 0.5", "0.5 to 0.7", "0.7 to 1"),
      `Correlation strength` = c("Very strong", "Strong", "Moderate", "Weak", "None", "Weak", "Moderate", "Strong", "Very strong"),
      `Correlation type` = c("Negative", "Negative", "Negative", "Negative", "Zero", "Positive", "Positive", "Positive", "Positive")
    )
    
    reactable(correlation_strength)
  })
  
  output$table2 <- renderReactable({
    type_of_relationship <- data.frame(
      `Correlation coefficient` = c("Pearson’s r", "Spearman’s rho", "Point-biserial", "Cramér’s V (Cramér’s φ)", "Kendall’s tau"),
      `Type of relationship` = c("Linear", "Non-linear", "Linear", "Non-linear", "Non-linear"),
      `Levels of measurement` = c("Two quantitative (interval or ratio) variables", "Two ordinal, interval or ratio variables", "One dichotomous (binary) variable and one quantitative (interval or ratio) variable", "Two nominal variables", "Two ordinal, interval or ratio variables"),
      `Data distribution` = c("Normal distribution", "Any distribution", "Normal distribution", "Any distribution", "Any distribution")
    )
    
    reactable(type_of_relationship)
  })
}


shinyApp(ui = ui, server = server)
