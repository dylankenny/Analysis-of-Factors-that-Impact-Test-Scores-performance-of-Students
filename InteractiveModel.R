library(shiny)
library(caret)
library(glmnet)
library(dplyr)
library(tibble)
library(shinybusy)
library(ggplot2)

# Load saved model artifacts
model <- readRDS("EN_ENG9.rds")
all_features <- readRDS("opt_ft_ENG9.rds")
feature_means <- readRDS("feature_means_ENG9.rds")

# Obtain variable importance and determine top features for the interface
importance_df <- varImp(model)$importance %>%
  tibble::rownames_to_column("Feature") %>%
  arrange(desc(Overall)) %>%
  head(14)

top_features <- importance_df$Feature

# Function to create a numeric input for each feature
create_input <- function(feat) {
  numericInput(
    inputId = feat,
    label = paste0(feat, " (200-1000)"),
    value = 600,
    min = 200,
    max = 1000,
    step = 50
  )
}

# Define the Shiny UI
ui <- fluidPage(
  # Display a full-page spinner on startup
  add_busy_spinner(
    spin = "cube-grid",
    color = "#FFFFFF",
    position = "full-page",
    timeout = 3000,
    margins = c(20,20)
  ),
  titlePanel("Grade Predictor"),
  sidebarLayout(
    sidebarPanel(
      h3("Student Performance Metrics"),
      lapply(top_features, create_input),
      actionButton(
        "predict",
        "Generate Prediction",
        class = "btn-success",
        width = "100%",
        style = "margin-top: 20px;"
      )
    ),
    mainPanel(
      h3("Prediction Analysis"),
      verbatimTextOutput("prediction"),
      plotOutput("importancePlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$predict, {
    # Show a modal spinner while processing the prediction
    show_modal_spinner(spin = "fading-circle", text = "Generating Prediction...")
    start_time <- Sys.time()
    
    # Create a new data frame for prediction from feature means
    prediction_data <- as.data.frame(t(feature_means))
    
    # Override with user inputs for the top influential features
    for(feat in top_features) {
      if(!is.null(input[[feat]])) {
        prediction_data[[feat]] <- as.numeric(input[[feat]])
      }
    }
    
    # Generate prediction using the final elastic net model
    predicted_grade <- predict(model, newdata = prediction_data)
    
    # Ensure the spinner is shown for at least 0.5 seconds
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if(elapsed < 0.5) { Sys.sleep(0.5 - elapsed) }
    
    output$prediction <- renderText({
      sprintf("Predicted Grade: %.1f", as.numeric(predicted_grade))
    })
    
    output$importancePlot <- renderPlot({
      ggplot(importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
        geom_bar(stat = "identity", fill = "#0072B2", width = 0.8) +
        coord_flip() +
        labs(title = "Feature Impact Analysis", x = "", y = "Importance") +
        theme_minimal()
    })
    
    remove_modal_spinner()
  })
}

shinyApp(ui = ui, server = server)

