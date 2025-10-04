# Forecaster_ShinyApp
A shiny app to input the univariate or multivariate dataset and obtain a time series model with forecasts

Note: 
* The dataset should contain the data in the ascending order of the date (ex: 2020, 2021, 2022,... ).
* The date should be in a separate column while each variable should be in a unique column.
* The variables taken to the model must be selected from the dropdown menu after uploading the dataset.
* The users can input a univariate dataset, then the app will fit a univariate time series model appropriately by optimizing the parameters.
* The users can input a multivariate dataset, then the app will fit a multivariate time series model appropriately by optimizing the parameters.

Limitations: 
* This shiny app is not addressing the situation where there is a seasonal effect.


