# Forecaster_ShinyApp
A shiny app to input a univariate or multivariate dataset and obtain a time series model with forecasts.

## Forecaster: Time Series Forecasting App: [Link](https://menashasenanayaka.shinyapps.io/forecaster_shinyapp/)

It is important to note that: 

* The dataset should contain the data in the ascending order of the date (ex: 2020, 2021, 2022, … ).
* The time point should be in a separate column, while each variable should be in a unique column.
* The dataset should have enough data (at least 150 data points) in order to conduct a time series analysis using this app.
* The users can input a univariate dataset, and then the app will fit a univariate time series model appropriately by optimizing the parameters.
* The users can input a multivariate dataset, and then the app will fit a multivariate time series model appropriately by optimizing the parameters.

Limitations:

* The shiny app might not work with all the dataset and the use will be limited.
* This shiny app is not addressing the situation where there is a seasonal effect.

Guidelines:

* First, a time series dataset should be used as the input. It should be a csv file.
* The required variable/s should be selected from the drop-down menu. There is no need to select the time point.
* The users must specify the frequency of the dataset and the forecast period. Usually, 80% is taken as the training dataset, but the users can specify the training data split as a percentage according to their requirements. Finally, click on “CONDUCT THE ANALYSIS”.
* The outputs will be displayed.

This shiny app will be helpful to someone who has a time series dataset and needs help with model fitting and forecasting. Anyone needing to conduct time series analysis using an interactive tool can take advantage of this shiny app. If the users have a good theoretical understanding about time series analysis, they can take the maximum benefit from this app.

