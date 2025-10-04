# ------------------------------------------------------------------------------

## Forecaster: Time Series Forecasting App

# ------------------------------------------------------------------------------

# libraries

library(shiny)
library(shinythemes)
library(forecast)
library(ggplot2)
library(vars)
library(tseries)  
library(urca)     
library(tsDyn)

# ------------------------------------------------------------------------------

# UI 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Forecaster: Time Series Forecasting App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # Multi-select for variables
      uiOutput("varSelectUI"),
      
      numericInput("frequency", 
                   "Frequency of the time series (12 for monthly, 4 for quarterly likewise):", 
                   value = 12, min = 1),
      numericInput("forecast_periods", 
                   "Forecast periods:", 
                   value = 12, min = 1),
      numericInput("train_split", 
                   "Training Data Split (as %):", 
                   value = 80, min = 50, max = 100),
      
      actionButton("analyze", "CONDUCT THE ANALYSIS"),
      
      # Note for task completion
      verbatimTextOutput("completionNote")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", 
                 h3("Dataset Preview"),
                 tableOutput("dataPreview")),
        
        tabPanel("Time Series Plot", 
                 h3("Time Series Plot"),
                 plotOutput("tsPlot")),
        
        tabPanel("Model Summary", 
                 h3("Fitted Model Summary"),
                 verbatimTextOutput("modelSummary")),
        
        tabPanel("Forecast", 
                 h3("Forecast Plot and Values"),
                 plotOutput("forecastPlot"),
                 tableOutput("forecastValues"))
      )
    )
  )
)

# ------------------------------------------------------------------------------

# Server logic
server <- function(input, output, session) {
  
  # Reactive function to load the dataset
  dataset <- reactive({
    req(input$datafile) 
    data <- read.csv(input$datafile$datapath)
    return(data)
  })
  
  # Generate variable selection UI based on dataset columns
  output$varSelectUI <- renderUI({
    req(dataset())  
    selectInput("variables", 
                "Select Variables for Time Series Analysis", 
                choices = colnames(dataset()), 
                selected = NULL, 
                multiple = TRUE)
  })
  
  # Preview of the dataset
  output$dataPreview <- renderTable({
    req(dataset())  
    head(dataset())
  })
  
  # Reactive function to create time series object
  ts_data <- reactive({
    req(dataset(), input$variables)  
    
    if (length(input$variables) == 0) {
      return(NULL)  # Return NULL if no variables selected
    }
    
    if (length(input$variables) == 1) {
      # Univariate time series for single column
      return(ts(dataset()[[input$variables]], frequency = input$frequency))
    } else {
      # Multivariate time series for multiple columns
      return(ts(dataset()[, input$variables], frequency = input$frequency))
    }
  })
  
  # Train-test split
  train_data <- reactive({
    tsd <- ts_data()
    req(tsd)  # Ensure tsd is not NULL
    if (is.matrix(tsd)) {
      split_idx <- round(nrow(tsd) * input$train_split / 100)
      return(tsd[1:split_idx, ])
    } else {
      split_idx <- round(length(tsd) * input$train_split / 100)
      return(tsd[1:split_idx])
    }
  })
  
  # Model fitting
  fitted_model <- eventReactive(input$analyze, {
    req(train_data()) 
    if (length(input$variables) == 1) {
      # Univariate ARIMA
      return(auto.arima(train_data()))
    } else {
      # Multivariate case: Test for stationarity
      adf_results <- lapply(1:ncol(train_data()), function(i) adf.test(train_data()[, i]))
      stationary <- sapply(adf_results, function(x) x$p.value < 0.05)
      
      if (all(stationary)) {
        # Fit VAR model if all series are stationary
        lag_selection <- VARselect(train_data(), lag.max = 5, type = "const")
        optimal_lag <- lag_selection$selection["AIC(n)"]
        return(VAR(train_data(), p = optimal_lag))
      } else {
        # Check for cointegration
        coint_test <- ca.jo(train_data(), type = "trace", ecdet = "const", K = 2)
        
        if (coint_test@teststat[1] > coint_test@cval[1, 1]) {
          # Fit VEC model if cointegration is present
          return(VECM(train_data(), r = 1, lag = 2))
        } else {
          # Fit VAR model if no cointegration
          lag_selection <- VARselect(train_data(), lag.max = 5, type = "const")
          optimal_lag <- lag_selection$selection["AIC(n)"]
          return(VAR(train_data(), p = optimal_lag))
        }
      }
    }
  })
  
  # Model summary output
  output$modelSummary <- renderPrint({
    req(fitted_model())  
    if (!is.null(fitted_model())) {
      summary(fitted_model())
    }
  })
  
  # Plot for the time series data
  output$tsPlot <- renderPlot({
    req(ts_data()) 
    if (length(input$variables) == 1) {
      autoplot(ts_data()) +
        ggtitle("Univariate Time Series Plot") +
        xlab("Time") + ylab("Value")
    } else {
      plot(ts_data(), main = "Multivariate Time Series Plot")
    }
  })
  
  # Forecasting
  output$forecastPlot <- renderPlot({
    req(fitted_model()) 
    
    # Extract original time series data for plotting
    original_data <- ts_data()
    
    if (length(input$variables) == 1) {
      # Univariate forecast plot
      forecasted_values <- forecast(fitted_model(), h = input$forecast_periods)
      
      # Combine historical and forecast data into a single time series for plotting
      combined_data <- ts(c(original_data, 
                            forecasted_values$mean), frequency = input$frequency)
      combined_lower <- ts(c(rep(NA, length(original_data)), 
                             forecasted_values$lower[, 2]), frequency = input$frequency)
      combined_upper <- ts(c(rep(NA, length(original_data)), 
                             forecasted_values$upper[, 2]), frequency = input$frequency)
      
      autoplot(combined_data) +
        autolayer(combined_lower, series = "Lower 95% CI", 
                  color = "red", linetype = "dashed") +
        autolayer(combined_upper, series = "Upper 95% CI", 
                  color = "red", linetype = "dashed") +
        ggtitle(paste("Univariate Forecast for next", 
                      input$forecast_periods, "periods")) +
        xlab("Time") + ylab("Values") +
        theme_minimal()
      
    } else {
      # Multivariate forecast plot
      forecasted_values <- tryCatch({
        predict(fitted_model(), n.ahead = input$forecast_periods)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(forecasted_values)) {
        # Extract forecasted means for each variable
        forecast_data <- do.call(cbind, lapply(forecasted_values$fcst, function(x) x[, 1]))
        combined_data <- ts(rbind(original_data, forecast_data), frequency = input$frequency)
        
        # Determine time series length for plotting forecasted and original data
        original_length <- nrow(original_data)
        forecast_length <- input$forecast_periods
        
        # Prepare a combined plot for multivariate data
        matplot(1:(original_length + forecast_length), 
                combined_data, type = "l", col = 1:ncol(combined_data),
                lty = 1, main = "Multivariate Forecast", 
                xlab = "Time", ylab = "Values")
        
        # Add a vertical line to separate historical data from forecasted data
        abline(v = original_length, 
               col = "blue", lwd = 2, lty = 2)
        
        legend("topright", legend = colnames(forecast_data), 
               col = 1:ncol(forecast_data), lty = 1)
      } else {
        showNotification("No valid forecast generated for multivariate model.", type = "error")
      }
    }
  })
  
  # Display forecast values
  output$forecastValues <- renderTable({
    req(fitted_model())
    
    if (length(input$variables) == 1) {
      # Univariate case: forecast
      forecasted_values <- forecast(fitted_model(), h = input$forecast_periods)
      
      # Generate the correct time index for the forecast
      forecast_time <- seq(from = end(train_data())[1] + 1/frequency(ts_data()), 
                           by = 1/frequency(ts_data()), 
                           length.out = input$forecast_periods)
      
      # Create a data frame for the forecast values and 95% CI
      forecasted_values_df <- data.frame(
        Forecast = forecasted_values$mean,
        Lower_95 = forecasted_values$lower[, 2],
        Upper_95 = forecasted_values$upper[, 2]
      )
      
      return(forecasted_values_df)
      
    } else {
      # Multivariate case: forecast
      forecasted_values <- predict(fitted_model(), 
                                   n.ahead = input$forecast_periods)
      
      # Extract only the forecasted values and 95% confidence intervals
      forecast_list <- lapply(forecasted_values$fcst, function(x) {
        data.frame(
          Forecast = x[, "fcst"],
          Lower_95 = x[, "lower"],
          Upper_95 = x[, "upper"]
        )
      })
      
      # Combine the forecasts for all variables into a single data frame
      forecasted_df <- do.call(cbind, forecast_list)
      return(forecasted_df)
    }
  })
  
  # Task completed note
  output$completionNote <- renderText({
    req(input$analyze)
    "ANALYSIS IS COMPLETED"
  })
}

# ------------------------------------------------------------------------------

# Run the Shiny app
shinyApp(ui = ui, server = server)

# ------------------------------------------------------------------------------



