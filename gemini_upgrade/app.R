library(shiny)
library(flexdashboard)
library(plotly)
library(dplyr)
library(h2o)

# Initialize H2O
h2o.init()

# Load the H2O model
model_path <- "/home/dsl/gemini/chrust_plus/gemini_upgrade/GBM_model_R_1758988233368_1"
model <- h2o.loadModel(model_path)

# Source the functions script globally so plotting functions are available
source("/home/dsl/gemini/chrust_plus/gemini_upgrade/functions.R")

# UI
ui <- fluidPage(
  
  # App title
  titlePanel("CHRUST_PLUS"),
  
  # Main panel for displaying outputs
  mainPanel(
    width = 12, # Make main panel take full width
    # Output: Tabset w/ plots
    tabsetPanel(type = "tabs",
                tabPanel("Energia bufora", 
                         fluidRow(
                           column(12,
                                  h3(htmlOutput("wood_needed_text_large")),
                                  plotlyOutput("qp_plot")
                           )
                         )
                ),
                tabPanel("Temperatury", plotlyOutput("temp_plot")),
                tabPanel("Profil bufora", plotlyOutput("buffer_plot")),
                tabPanel("Krzywa grzewcza", plotlyOutput("krzywe_plot"))
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive file reader to monitor the pre-processed data file
  file_data <- reactiveFileReader(15 * 60 * 1000, NULL, "/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data_for_app.feather", function(x) feather::read_feather(x))
  
  # Reactive expression to get the latest processed data
  current_dt_reactive <- reactive({
    req(file.exists("/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data_for_app.feather"), message = "ml_model_data_for_app.feather not found. Run run_etl.R first.")
    file_data()
  })
  
  # Reactive expression for the latest data point, ensuring it's not empty and has required columns
  latest_data_point_reactive <- reactive({
    req(nrow(current_dt_reactive()) > 0, message = "No data available from ml_model_data_for_app.feather") # Ensure dt is not empty
    
    # Get the last row
    latest_point <- current_dt_reactive()[nrow(current_dt_reactive()), ]
    
    # Ensure all predictors are present and handle NAs for prediction
    predictors <- c("temp_home", "temp", "temp_piec", "temp_co", "bufor_top", "bufor_mid1", "bufor_mid2", "bufor_bottom", "WABT", "heating_cycle", "discharging_cycle", "heat_demand_kwh", "temp_lag1", "heat_demand_kwh_lag1")
    
    # Check if all predictors exist in latest_point
    req(all(predictors %in% colnames(latest_point)), message = "Missing predictors in latest data point.") # Ensure all predictors are in the data
    
    # Replace NAs in predictors with 0 or a reasonable default for prediction
    for (p in predictors) {
      if (is.na(latest_point[[p]])) {
        latest_point[[p]] <- 0 # Basic imputation: replace NA with 0
      }
    }
    
    latest_point
  })

  # Render the plots
  output$qp_plot <- renderPlotly({
    req(nrow(current_dt_reactive()) > 0, message = "No data for qp_plot.") # Ensure dt is not empty before plotting
    
    dt_for_plots <- current_dt_reactive()
    qp_plot_obj <- plot_energy_buffer(dt_for_plots)
    
    plotly::ggplotly(qp_plot_obj) 
  })
  
  output$temp_plot <- renderPlotly({
    req(nrow(current_dt_reactive()) > 0)
    dt_for_plots <- current_dt_reactive()
    temp_plot_obj <- plot_temperatures(dt_for_plots)
    plotly::ggplotly(temp_plot_obj, dynamicTicks = TRUE) %>%
      layout(legend = list(orientation = 'h'))
  })
  
  output$buffer_plot <- renderPlotly({
    req(nrow(current_dt_reactive()) > 0)
    dt_for_plots <- current_dt_reactive()
    buffer_plot_obj <- plot_buffer_profile(dt_for_plots)
    plotly::ggplotly(buffer_plot_obj, dynamicTicks = TRUE) %>%
      layout(legend = list(orientation = 'h'))
  })
  
  output$krzywe_plot <- renderPlotly({
    req(nrow(current_dt_reactive()) > 0)
    dt_for_plots <- current_dt_reactive()
    krzywe_plot_obj <- plot_heating_curve(dt_for_plots)
    plotly::ggplotly(krzywe_plot_obj)
  })
  
  # Text prediction
  output$wood_needed_text_large <- renderUI({
    latest_data_point <- latest_data_point_reactive()
    
    latest_data_h2o <- as.h2o(latest_data_point)
    prediction <- h2o.predict(model, latest_data_h2o)
    wood_needed <- as.data.frame(prediction)$predict
    
    HTML(paste0("<span style='font-size: 24px;'>Sugerowana ilość drewna: ", round(wood_needed, 2), " kg</span>"))
  })
  
}

# Create Shiny app
shinyApp(ui, server)
