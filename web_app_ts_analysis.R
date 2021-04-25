library(shiny)
library(tidyverse)
library(readxl) # reading excel files
library(tseries) # converting data to time-series
library(forecast)
library(lubridate) # dealing with time-series dates
library(lmtest) # coeftest
library(broom) # cleaning statistical outputs

ui <- 
  fluidPage(
    ## Uploading data
    h3("Data"),
    h4("Upload Data"),
    h5("Upload CSV/Excel File"),
    h6("Make sure all you data (including external variables) are in one table / file."),
    h6("Make sure there are no gaps in your dates. There can be gaps in your data"),
    fileInput(inputId = "data_file", 
              label = NULL, 
              accept = c("text/csv", ".csv", ".xlsx",
                         "text/comma-separated-values,text/plain")),
    ## Displaying data
    tableOutput("data_snapshot"),
    
    ## Converting data to time series
    h4("Converting Data to Time-Series"),
    h5("Select Date Range of Your Data"),
    h6("These wil be the date of the first data in your file"),
    h6("If no date is set, the start date will be the default date shown below"),
    dateInput(inputId = "data_start_date",
                   label = NULL,
                   format = "yyyy/mm/dd"),
    h5("Choose Frequency of Your Data"),
    radioButtons(inputId = "ts_frequency", 
                 label = NULL,
                 choices = c("daily" = "day", 
                             "weekly" = "week",
                             "monthly" = "month",
                             "quarterly" = "quarter",
                             "yearly" = "year")),
    
    ## Selecting data for analysis
    h4("Selecting the Series to Conduct Time-Series Analysis"),
    selectInput(inputId = "select_variable", 
                label = h5("Select Variable to Analyse"), 
                choices = "(none)", multiple = FALSE),
    actionButton(inputId = "update_data_button", 
                 label = "Select", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    textOutput("selected_var_name"),
    
    ## Stationarity test
    h4("Preliminary Analysis - Stationarity Tests and ACF / PACF Plots"),
    radioButtons(inputId = "stat_test_type", 
                 label = h5("Choose Stationarity Test"),
                 choices = c("Augmented Dickey-Fuller (ADF)" = "ADF", 
                             "Phillips-Perron (PP)" = "PP",
                             "Kwiatkowski-Phillips-Schmidt-Shin (KPSS)" = "KPSS")),
    actionButton(inputId = "stat_test_button", 
                 label = "Run Test", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    verbatimTextOutput("stat_test_result"),
    
    ## ACF and PACF Plots
    actionButton(inputId = "ACF_plot_button", 
                 label = "Plot ACF", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    plotOutput("ACF_chart"),
    actionButton(inputId = "PACF_plot_button", 
                 label = "Plot PACF", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    plotOutput("PACF_chart"),
    
    ## Exponential smoothing models
    h3("In-Sample Prediction"),
    h4("Exponential Smoothing Models"),
    radioButtons(inputId = "es_model_type", 
                 label = h5("Choose Exponential Smothing (ES) Model"),
                 choices = c("Simple Exponential Smoothing" = "SES", 
                             "Holt's Linear Method" = "Holt",
                             "Damped Holt's Method" = "Holtd",
                             "Additive Holt-Winter's" = "HWa",
                             "Multiplicative Holt-Winter's" = "HWm",
                             "Damped Holt-Winter's" = "HWmd")),
    actionButton(inputId = "es_model_button", 
                 label = "Run Exponential Smoothing Model", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    plotOutput("ets_chart"),
    tableOutput("ets_accuracies"),
    
    # In-sample prediction
    ## Auto-arima / arimax
    h3("In-Sample Prediction"),
    h4("Auto-ARIMA / ARIMAX"),
    h6("App Selects the ARIMA / ARIMAX model that gives the lowest Information Criterion (IC) Value"),
    radioButtons(inputId = "choose_auto_arima_arimax", 
                 label = h5("Choose Auto ARIMA or ARIMAX"),
                 choices = c("Auto ARIMA Analysis" = "auto_arima", 
                             "Auto ARIMAX Analysis" = "auto_arimax")),
    radioButtons(inputId = "select_IC", 
                 label = h5("Choose Information Criterion"),
                 choices = c("Akaike Information Criterion (AIC)" = "aic", 
                             "Akaike Information Criterion corrected (AICc)" = "aicc",
                             "Bayesian Information Criterion (BIC)" = "bic")),
    selectInput(inputId = "select_arimax_variable", 
                label = h5("Select ARIMAX External Variable"), 
                choices = "(none)", multiple = TRUE),
    actionButton(inputId = "update_ext_var_button", 
                 label = "Update External Variables", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    actionButton(inputId = "run_arima_arimax_button", 
                 label = "Run ARIMA/ARIMAX Analysis", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    textOutput("arima_model_des"),
    plotOutput("auto_arima_chart"),
    tableOutput("auto_arima_coef"),
    tableOutput("auto_arima_accuracies"),
    
    ## Manual arima models
    h4("Manual ARIMA / ARIMAX"),
    h6("Define Your Own p, d, and q Parameters"),
    radioButtons(inputId = "choose_man_arima_arimax", 
                 label = h5("Choose Manual ARIMA or ARIMAX"),
                 choices = c("ARIMA Analysis" = "man_arima", 
                             "ARIMAX Analysis" = "man_arimax")),
    numericInput(inputId = "manual_p_parameter",
                 label = h5("Choonse number of autoregressive terms (p)"),
                 value = 0, min = 0, max = 10, step = 1),
    numericInput(inputId = "manual_d_parameter",
                 label = h5("Choose number of differences (d)"),
                 value = 0, min = 0, max = 4, step = 1),
    numericInput(inputId = "manual_q_parameter",
                 label = h5("Choose number of lagged forecast errors (q)"),
                 value = 0, min = 0, max = 10, step = 1),
    selectInput(inputId = "select_man_arimax_variable", 
                label = h5("Select ARIMAX External Variable"), 
                choices = "(none)", multiple = TRUE),
    actionButton(inputId = "update_man_ext_var_button", 
                 label = "Update External Variables", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    actionButton(inputId = "run_man_arima_arimax_button", 
                 label = "Run Manual ARIMA/ARIMAX Analysis", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    textOutput("man_arima_model_des"),
    plotOutput("man_arima_chart"),
    tableOutput("man_arima_coef"),
    tableOutput("man_arima_accuracies"),
    
    
    # Out-of-sample forecast
    ## Exponential smoothing models
    h3("Out-of-Sample Forecasting"),
    h4("Exponential Smoothing Models"),
    numericInput(inputId = "forecast_period",
                 label = h5("Choose Forecast Period"),
                 value = 0, min = 0, step = 1),
    radioButtons(inputId = "es_model_type_fore", 
                 label = h5("Choose Exponential Smothing (ES) Model"),
                 choices = c("Simple Exponential Smoothing" = "SES", 
                             "Holt's Linear Method" = "Holt",
                             "Damped Holt's Method" = "Holtd",
                             "Additive Holt-Winter's" = "HWa",
                             "Multiplicative Holt-Winter's" = "HWm",
                             "Damped Holt-Winter's" = "HWmd")),
    actionButton(inputId = "es_model_button_fore", 
                 label = "Run Exponential Smoothing Model", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    plotOutput("ets_chart_fore"),
    tableOutput("ets_accuracies_fore"),
    
    ## Auto-arima / arimax models
    h4("Auto-ARIMA / ARIMAX"),
    h6("App Selects the ARIMA / ARIMAX model that gives the lowest Information Criterion (IC) Value"),
    radioButtons(inputId = "choose_auto_arima_arimax_fore", 
                 label = h5("Choose Auto ARIMA or ARIMAX"),
                 choices = c("Auto ARIMA Analysis" = "auto_arima", 
                             "Auto ARIMAX Analysis" = "auto_arimax")),
    radioButtons(inputId = "select_IC_fore", 
                 label = h5("Choose Information Criterion"),
                 choices = c("Akaike Information Criterion (AIC)" = "aic", 
                             "Akaike Information Criterion corrected (AICc)" = "aicc",
                             "Bayesian Information Criterion (BIC)" = "bic")),
    selectInput(inputId = "select_arimax_variable_fore", 
                label = h5("Select ARIMAX External Variable"), 
                choices = "(none)", multiple = TRUE),
    actionButton(inputId = "update_ext_var_button_fore", 
                 label = "Update External Variables", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    numericInput(inputId = "forecast_period_auto_ar",
                 label = h5("Choose Forecast Period"),
                 value = 0, min = 0, step = 1),
    actionButton(inputId = "run_arima_arimax_button_fore", 
                 label = "Run ARIMA/ARIMAX Analysis", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    textOutput("arima_model_des_fore"),
    plotOutput("auto_arima_chart_fore"),
    tableOutput("auto_arima_coef_fore"),
    tableOutput("auto_arima_accuracies_fore"),
    
    ## Manual arima models
    h4("Manual ARIMA / ARIMAX"),
    h6("Define Your Own p, d, and q Parameters"),
    radioButtons(inputId = "choose_man_arima_arimax_fore", 
                 label = h5("Choose Manual ARIMA or ARIMAX"),
                 choices = c("ARIMA Analysis" = "man_arima", 
                             "ARIMAX Analysis" = "man_arimax")),
    numericInput(inputId = "manual_p_parameter_fore",
                 label = h5("Choonse number of autoregressive terms (p)"),
                 value = 0, min = 0, max = 10, step = 1),
    numericInput(inputId = "manual_d_parameter_fore",
                 label = h5("Choose number of differences (d)"),
                 value = 0, min = 0, max = 4, step = 1),
    numericInput(inputId = "manual_q_parameter_fore",
                 label = h5("Choose number of lagged forecast errors (q)"),
                 value = 0, min = 0, max = 10, step = 1),
    selectInput(inputId = "select_man_arimax_variable_fore", 
                label = h5("Select ARIMAX External Variable"), 
                choices = "(none)", multiple = TRUE),
    actionButton(inputId = "update_man_ext_var_button_fore", 
                 label = "Update External Variables", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    numericInput(inputId = "forecast_period_man_ar",
                 label = h5("Choose Forecast Period"),
                 value = 0, min = 0, step = 1),
    actionButton(inputId = "run_man_arima_arimax_button_fore", 
                 label = "Run Manual ARIMA/ARIMAX Analysis", 
                 class = "btn-primary", style = 'padding:4px; font-size:100%'),
    textOutput("man_arima_model_des_fore"),
    plotOutput("man_arima_chart_fore"),
    tableOutput("man_arima_coef_fore"),
    tableOutput("man_arima_accuracies_fore")
    
    
    )


server <- function(session, input, output){
  ## identify the uploaded file format and use the relevant function to read it 
  data_table <- reactive({
    req(input$data_file)
    if(endsWith(input$data_file$name, '.csv')) {
      data <- read_csv(input$data_file$datapath)
    } else {
      data <- read_xlsx(input$data_file$datapath)
      }
    return(data)
  })
  
   ## Display the uploaded file
  output$data_snapshot <- renderTable({
    head(data_table(), n = 6)
    })
  
  ## Updating the data set to reflect only the selected columns
  selected_data <- eventReactive(input$update_data_button, {
    req(data_table())
    if(is.null(input$select_variable)) data_table() 
      else data_table()[, colnames(data_table()) %in% input$select_variable]
      })
  observeEvent(data_table(), {
    updateSelectInput(session, "select_variable", 
                      choices = colnames(data_table()))
    })
  
  ## Identifying the data selected
  output$selected_var_name <- eventReactive(input$update_data_button, {
    paste("Variable", input$select_variable, "selected")
  })
  
  ## Running the stationarity tests based on selection
  output$stat_test_result <- eventReactive(input$stat_test_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency) %>%
      na.remove()
    variable_name <- colnames(selected_data())
    if(input$stat_test_type == "ADF"){
      test_summ_adf <- adf.test(working_data, alternative = "explosive")
      test_pval <- formatC(test_summ_adf$p.value, format = "f", digits = 4)
      if(test_pval < 0.05) test_res <- "non-stationary"
      else test_res <- "stationary"
      paste("series", variable_name, "has an ADF test p-value of", 
            test_pval, "and is", test_res)
      } else if(input$stat_test_type == "PP"){
        test_summ_pp <- pp.test(working_data, alternative = "explosive")
        test_pval <- formatC(test_summ_pp$p.value, format = "f", digits = 4)
        if(test_pval < 0.05) test_res <- "non-stationary"
        else test_res <- "stationary"
        paste("series", variable_name, "has a PP test p-value of", 
              test_pval, "and is", test_res)
        } else {
          test_summ_kpss <- kpss.test(working_data, null = "Level")
          test_pval <- formatC(test_summ_kpss$p.value, format = "f", digits = 4)
          if(test_pval < 0.05) test_res <- "non-stationary"
          else test_res <- "stationary"
          paste("series", variable_name, "has a KPSS test p-value of", 
                test_pval, "and is", test_res)
          }
    })
  
  ## ACF and PACF plots
  ACF_plot <- eventReactive(input$ACF_plot_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency) %>%
      na.remove()
    acf(working_data, main = "")
    })
  output$ACF_chart <- renderPlot(ACF_plot())
  
  PACF_plot <- eventReactive(input$PACF_plot_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency) %>%
      na.remove()
    pacf(working_data, main = "")
    })
  output$PACF_chart <- renderPlot(PACF_plot())
  
  ## Exponential smoothing models
  ets_model <- eventReactive(input$es_model_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    if(input$es_model_type == "SES") {
      ses_model <- ets(working_data, model = "ZNN",
                       damped = NULL, na.action = "na.interp")
      acc_table <- accuracy(ses_model)
      output_list <- list(actual = working_data, 
                          predicted = ses_model$fitted,
                          errors = ses_model$residuals,
                          acc_table = acc_table)
    } else if(input$es_model_type == "Holt"){
      holt_model <- ets(working_data, model = "ZAN",
                        damped = FALSE, na.action = "na.interp")
      acc_table <- accuracy(holt_model)
      output_list <- list(actual = working_data, 
                          predicted = holt_model$fitted,
                          errors = holt_model$residuals,
                          acc_table = acc_table)
    } else if(input$es_model_type == "Holtd"){
      holtd_model <- ets(working_data, model = "ZAN",
                         damped = TRUE, na.action = "na.interp")
      acc_table <- accuracy(holtd_model)
      output_list <- list(actual = working_data, 
                          predicted = holtd_model$fitted,
                          errors = holtd_model$residuals,
                          acc_table = acc_table)
    } else if(input$es_model_type == "HWa"){
      HWa_model <- ets(working_data, model = "ZAA",
                       damped = FALSE, na.action = "na.interp")
      acc_table <- accuracy(HWa_model)
      output_list <- list(actual = working_data, 
                          predicted = HWa_model$fitted,
                          errors = HWa_model$residuals,
                          acc_table = acc_table)
    } else if(input$es_model_type == "HWm"){
      HWm_model <- ets(working_data, model = "ZAM",
                       damped = FALSE, na.action = "na.interp")
      acc_table <- accuracy(HWm_model)
      output_list <- list(actual = working_data, 
                          predicted = HWm_model$fitted,
                          errors = HWm_model$residuals,
                          acc_table = acc_table)
    } else {
      HWmd_model <- ets(working_data, model = "ZAM",
                        damped = TRUE, na.action = "na.interp")
      acc_table <- accuracy(HWmd_model)
      output_list <- list(actual = working_data, 
                          predicted = HWmd_model$fitted,
                          errors = HWmd_model$residuals,
                          acc_table = acc_table)
      }
    return(output_list)
    })
  
    ### Plot forecast vs actual for ets 
  output$ets_chart <- renderPlot({
    Date <- seq(as.Date(input$data_start_date), by = input$ts_frequency, 
                length.out = length(ets_model()$actual))
    plot_data <- cbind(Date, ets_model()$actual, ets_model()$predicted, 
                       ets_model()$errors)
    plot_data <- as.data.frame(plot_data)
    colnames(plot_data) <- c("Date", "actual", "predicted", "errors")
    plot_data$Date <- as.Date(plot_data$Date, format = "%Y/%m/%d",
                              origin = "1970/01/01")
    plot_data_long <- plot_data %>%
      pivot_longer(cols = 2:4, names_to = "output_type", 
                   values_to = "value")
    plot_data_long %>% 
      filter(output_type %in% c("actual", "predicted")) %>%
      ggplot(aes(x = Date, y = value, color = output_type)) +
      geom_line() +
      scale_x_date(date_labels = "%d %b %Y")
    })
  
    ### show prediction accuracies
  output$ets_accuracies <- renderTable(ets_model()$acc_table)
  
  ## Auto-arima models
  observeEvent(input$choose_auto_arima_arimax, {
    if(input$choose_auto_arima_arimax == "auto_arima") {
      updateSelectInput(session, "select_arimax_variable", 
                        choices = "none")
    } else {updateSelectInput(session, "select_arimax_variable", 
                              choices = colnames(data_table()))
    }})
  
    ### generate xreg matrix
  external_data <- eventReactive(input$update_ext_var_button, {
    req(data_table())
    if(is.null(input$select_arimax_variable)) data_table()
    else data_table()[, colnames(data_table()) %in% input$select_arimax_variable]
    })
  
    ### Run auto-arima / arimax
  auto_arima_model <- eventReactive(input$run_arima_arimax_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    if(input$choose_auto_arima_arimax == "auto_arima") {
      auto_arima_model <- auto.arima(working_data,
                                     max.p = 8, max.q = 8, max.d = 2,
                                     method = "ML", approximation = FALSE,
                                     ic = input$select_IC, stepwise = FALSE)
      acc_table <- accuracy(auto_arima_model)
      regr_coef <- coeftest(auto_arima_model)
      output_list <- list(actual = working_data, 
                          predicted = auto_arima_model$fitted,
                          errors = auto_arima_model$residuals,
                          p = auto_arima_model$arma[1],
                          d = auto_arima_model$arma[6],
                          q = auto_arima_model$arma[2],
                          acc_table = acc_table,
                          regr_coef = regr_coef)
      } else {
        if(input$ts_frequency == "day") frequency <- 365
        else if (input$ts_frequency == "week") frequency <- 365/7
        else if (input$ts_frequency == "month") frequency <- 12
        else if (input$ts_frequency == "quarter") frequency <- 4
        else frequency <- 1
        xreg_data <-
          ts(external_data(),
             start = c(as.numeric(format(input$data_start_date, "%Y")), 
                       yday(as.Date(input$data_start_date, 
                                    format = "%Y/%m/%d"))),
             frequency = frequency)
        xreg <- data.matrix(xreg_data)
        auto_arimax_model <- auto.arima(working_data,
                                        max.p = 8, max.q = 8, max.d = 2,
                                        method = "ML", approximation = FALSE,
                                        ic = input$select_IC, stepwise = FALSE,
                                        xreg = xreg)
        acc_table <- accuracy(auto_arimax_model)
        regr_coef <- coeftest(auto_arimax_model)
        output_list <- list(actual = working_data,
                            predicted = auto_arimax_model$fitted,
                            errors = auto_arimax_model$residuals,
                            p = auto_arimax_model$arma[1],
                            d = auto_arimax_model$arma[6],
                            q = auto_arimax_model$arma[2],
                            acc_table = acc_table,
                            regr_coef = regr_coef)
        }
    return(output_list)
    })
  
    ### Plot forecast vs actual for auto-arima
  output$auto_arima_chart <- renderPlot({
    Date <- seq(as.Date(input$data_start_date), by = input$ts_frequency, 
                length.out = length(auto_arima_model()$actual))
    plot_data <- cbind(Date, auto_arima_model()$actual, 
                       auto_arima_model()$predicted, auto_arima_model()$errors)
    plot_data <- as.data.frame(plot_data)
    colnames(plot_data) <- c("Date", "actual", "predicted", "errors")
    plot_data$Date <- as.Date(plot_data$Date, format = "%Y/%m/%d",
                              origin = "1970/01/01")
    plot_data_long <- plot_data %>%
      pivot_longer(cols = 2:4, names_to = "output_type", 
                   values_to = "value")
    plot_data_long %>% 
      filter(output_type %in% c("actual", "predicted")) %>%
      ggplot(aes(x = Date, y = value, color = output_type)) +
      geom_line() +
      scale_x_date(date_labels = "%d %b %Y")
  })
  
    ### show arima p, d, and q parameters
  output$arima_model_des <- eventReactive(input$run_arima_arimax_button, {
    paste("ARIMA(", auto_arima_model()$p, ",", auto_arima_model()$d, ",", 
          auto_arima_model()$q, ") model generated")
    })
  
    ### Show variable coefficients
  output$auto_arima_coef <- renderTable(tidy(auto_arima_model()$regr_coef))
  
    ### show prediction accuracies
  output$auto_arima_accuracies <- renderTable(auto_arima_model()$acc_table)
  
  
  ## Manual arima models
  observeEvent(input$choose_man_arima_arimax, {
    if(input$choose_man_arima_arimax == "man_arima") {
      updateSelectInput(session, "select_man_arimax_variable", 
                        choices = "none")
    } else {updateSelectInput(session, "select_man_arimax_variable", 
                              choices = colnames(data_table()))
    }})
  
  ### generate xreg matrix
  man_external_data <- eventReactive(input$update_man_ext_var_button, {
    req(data_table())
    if(is.null(input$select_man_arimax_variable)) data_table()
    else data_table()[, colnames(data_table()) %in% 
                        input$select_man_arimax_variable]
  })
  
  ### Run manual arima / arimax
  man_arima_model <- eventReactive(input$run_man_arima_arimax_button, {
    req(selected_data())
    if(input$ts_frequency == "day") frequency <- 365
    else if (input$ts_frequency == "week") frequency <- 365/7
    else if (input$ts_frequency == "month") frequency <- 12
    else if (input$ts_frequency == "quarter") frequency <- 4
    else frequency <- 1
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    if(input$choose_man_arima_arimax == "man_arima") {
      man_arima_model <- Arima(working_data,
                               order = c(input$manual_p_parameter,
                                         input$manual_d_parameter,
                                         input$manual_q_parameter))
      acc_table <- accuracy(man_arima_model)
      regr_coef <- coeftest(man_arima_model)
      output_list <- list(actual = working_data, 
                          predicted = man_arima_model$fitted,
                          errors = man_arima_model$residuals,
                          p = input$manual_p_parameter,
                          d = input$manual_d_parameter,
                          q = input$manual_q_parameter,
                          acc_table = acc_table,
                          regr_coef = regr_coef)
    } else {
      if(input$ts_frequency == "day") frequency <- 365
      else if (input$ts_frequency == "week") frequency <- 365/7
      else if (input$ts_frequency == "month") frequency <- 12
      else if (input$ts_frequency == "quarter") frequency <- 4
      else frequency <- 1
      xreg_data <-
        ts(man_external_data(),
           start = c(as.numeric(format(input$data_start_date, "%Y")), 
                     yday(as.Date(input$data_start_date, 
                                  format = "%Y/%m/%d"))),
           frequency = frequency)
      xreg <- data.matrix(xreg_data)
      man_arimax_model <- Arima(working_data,
                                order = c(input$manual_p_parameter,
                                          input$manual_d_parameter,
                                          input$manual_q_parameter),
                                xreg = xreg)
      acc_table <- accuracy(man_arimax_model)
      regr_coef <- coeftest(man_arimax_model)
      output_list <- list(actual = working_data,
                          predicted = man_arimax_model$fitted,
                          errors = man_arimax_model$residuals,
                          p = input$manual_p_parameter,
                          d = input$manual_d_parameter,
                          q = input$manual_q_parameter,
                          acc_table = acc_table,
                          regr_coef = regr_coef)
    }
    return(output_list)
  })
  
  ### Plot forecast vs actual for manual arima
  output$man_arima_chart <- renderPlot({
    Date <- seq(as.Date(input$data_start_date), by = input$ts_frequency, 
                length.out = length(man_arima_model()$actual))
    plot_data <- cbind(Date, man_arima_model()$actual, 
                       man_arima_model()$predicted, man_arima_model()$errors)
    plot_data <- as.data.frame(plot_data)
    colnames(plot_data) <- c("Date", "actual", "predicted", "errors")
    plot_data$Date <- as.Date(plot_data$Date, format = "%Y/%m/%d",
                              origin = "1970/01/01")
    plot_data_long <- plot_data %>%
      pivot_longer(cols = 2:4, names_to = "output_type", 
                   values_to = "value")
    plot_data_long %>% 
      filter(output_type %in% c("actual", "predicted")) %>%
      ggplot(aes(x = Date, y = value, color = output_type)) +
      geom_line() +
      scale_x_date(date_labels = "%d %b %Y")
  })
  
  ### show defined p, d, and q parameters
  output$man_arima_model_des <- 
    eventReactive(input$run_man_arima_arimax_button, {
    paste("ARIMA(", input$manual_p_parameter, ",", input$manual_d_parameter, 
          ",", input$manual_q_parameter, ") model")
  })
  
  ### Show variable coefficients
  output$man_arima_coef <- renderTable(tidy(man_arima_model()$regr_coef))
  
  ### show prediction accuracies
  output$man_arima_accuracies <- renderTable(man_arima_model()$acc_table)
  
  
  # Out of sample forecasting
  ## Exponential smoothing models
  ets_fore <- eventReactive(input$es_model_button_fore, {
    req(selected_data())
    
    ## Sort data by frequency type
    if(input$ts_frequency == "day") {
      frequency <- 365
      period_1 <- days(1)
    } else if (input$ts_frequency == "week") {
      frequency <- 365/7
      period_1 <- weeks(1)
    } else if (input$ts_frequency == "month") {
      frequency <- 12
      period_1 <- months(1)
    } else if (input$ts_frequency == "quarter") {
      frequency <- 4
      period_1 <- months(3)
    } else {
      frequency <- 1
      period_1 <- years(1)
    }
    
    ## Slicing the dataset into training set and forecast set
    length_full <- nrow(selected_data())
    length_train <- length_full - input$forecast_period
    
    training_data <- selected_data()[1:length_train,]
    forecast_data <- selected_data()[(length_train + period_1):length_full,]
    
    fore_start_date <- ymd(input$data_start_date) + days(length_train)
    
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    training_ts <- 
      ts(training_data,
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    forecast_ts <- 
      ts(forecast_data,
         start = c(as.numeric(format(fore_start_date, "%Y")), 
                   yday(as.Date(fore_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    ## Running the exponential smoothing models
    if(input$es_model_type_fore == "SES") {
      ses_model <- ets(training_ts, model = "ZNN",
                       damped = NULL, na.action = "na.interp")
      forecast <- forecast(ses_model, h = input$forecast_period)
      acc_table <- accuracy(forecast)
      output_list <- list(actual = working_data,
                          predicted = ses_model$fitted,
                          errors = ses_model$residuals,
                          acc_table = acc_table,
                          forecast = forecast)
      } else if(input$es_model_type_fore == "Holt"){
        holt_model <- ets(training_ts, model = "ZAN",
                          damped = FALSE, na.action = "na.interp")
        forecast <- forecast(holt_model, h = input$forecast_period)
        acc_table <- accuracy(forecast)
        output_list <- list(actual = working_data, 
                            predicted = holt_model$fitted,
                            errors = holt_model$residuals,
                            acc_table = acc_table,
                            forecast = forecast)
      } else if(input$es_model_type_fore == "Holtd"){
        holtd_model <- ets(training_ts, model = "ZAN",
                           damped = TRUE, na.action = "na.interp")
        forecast <- forecast(holtd_model, h = input$forecast_period)
        acc_table <- accuracy(forecast)
        output_list <- list(actual = working_data, 
                            predicted = holtd_model$fitted,
                            errors = holtd_model$residuals,
                            acc_table = acc_table,
                            forecast = forecast)
      } else if(input$es_model_type_fore == "HWa"){
        HWa_model <- ets(training_ts, model = "ZAA",
                         damped = FALSE, na.action = "na.interp")
        forecast <- forecast(HWa_model, h = input$forecast_period)
        acc_table <- accuracy(forecast)
        output_list <- list(actual = working_data, 
                            predicted = HWa_model$fitted,
                            errors = HWa_model$residuals,
                            acc_table = acc_table,
                            forecast = forecast)
      } else if(input$es_model_type_fore == "HWm"){
        HWm_model <- ets(training_ts, model = "ZAM",
                         damped = FALSE, na.action = "na.interp")
        forecast <- forecast(HWm_model, h = input$forecast_period)
        acc_table <- accuracy(forecast)
        output_list <- list(actual = working_data, 
                            predicted = HWm_model$fitted,
                            errors = HWm_model$residuals,
                            acc_table = acc_table,
                            forecast = forecast)
      } else {
        HWmd_model <- ets(training_ts, model = "ZAM",
                          damped = TRUE, na.action = "na.interp")
        forecast <- forecast(HWmd_model, h = input$forecast_period)
        acc_table <- accuracy(forecast)
        output_list <- list(actual = working_data, 
                            predicted = HWmd_model$fitted,
                            errors = HWmd_model$residuals,
                            acc_table = acc_table,
                            forecast = forecast)
      }
    return(output_list)
  })
  
  ### Plot forecast vs actual for ets 
  output$ets_chart_fore <- renderPlot({
    plot(ets_fore()$forecast)
    lines(ets_fore()$actual, col = "black")
  })
  
  ### show prediction accuracies
  output$ets_accuracies_fore <- renderTable(ets_fore()$acc_table)
  
  ## Auto-arima models
  observeEvent(input$choose_auto_arima_arimax_fore, {
    if(input$choose_auto_arima_arimax_fore == "auto_arima") {
      updateSelectInput(session, "select_arimax_variable_fore", 
                        choices = "none")
    } else {updateSelectInput(session, "select_arimax_variable_fore", 
                              choices = colnames(data_table()))
    }})
  
  ### generate xreg matrix
  external_data_fore <- eventReactive(input$update_ext_var_button_fore, {
    req(data_table())
    if(is.null(input$select_arimax_variable_fore)) data_table()
    else data_table()[, colnames(data_table()) %in% 
                        input$select_arimax_variable_fore]
  })
  
  ### Run auto-arima / arimax
  auto_arima_fore <- eventReactive(input$run_arima_arimax_button_fore, {
    req(selected_data())
    
    ## Sort data by frequency type
    if(input$ts_frequency == "day") {
      frequency <- 365
      period_1 <- days(1)
    } else if (input$ts_frequency == "week") {
      frequency <- 365/7
      period_1 <- weeks(1)
    } else if (input$ts_frequency == "month") {
      frequency <- 12
      period_1 <- months(1)
    } else if (input$ts_frequency == "quarter") {
      frequency <- 4
      period_1 <- months(3)
    } else {
      frequency <- 1
      period_1 <- years(1)
    }
    
    ## Slicing the dataset into training set and forecast set
    length_full <- nrow(selected_data())
    length_train <- length_full - input$forecast_period_auto_ar
    
    training_data <- selected_data()[1:length_train,]
    forecast_data <- selected_data()[(length_train + period_1):length_full,]
    
    fore_start_date <- ymd(input$data_start_date) + days(length_train)
    
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    training_ts <- 
      ts(training_data,
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    forecast_ts <- 
      ts(forecast_data,
         start = c(as.numeric(format(fore_start_date, "%Y")), 
                   yday(as.Date(fore_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    ## Running auto-arima / arimax
    if(input$choose_auto_arima_arimax_fore == "auto_arima") {
      auto_arima_train <- auto.arima(training_ts,
                                     max.p = 8, max.q = 8, max.d = 2,
                                     method = "ML", approximation = FALSE,
                                     ic = input$select_IC, stepwise = FALSE)
      forecast <- forecast(auto_arima_train, h = input$forecast_period_auto_ar)
      acc_table <- accuracy(forecast)
      regr_coef <- coeftest(auto_arima_train)
      output_list <- list(actual = working_data, 
                          predicted = auto_arima_train$fitted,
                          errors = auto_arima_train$residuals,
                          p = auto_arima_train$arma[1],
                          d = auto_arima_train$arma[6],
                          q = auto_arima_train$arma[2],
                          acc_table = acc_table,
                          regr_coef = regr_coef,
                          forecast = forecast)
    } else {
      xreg_training_data <- external_data_fore()[1:length_train,]
      xreg_forecast_data <- 
        external_data_fore()[(length_train + period_1):length_full,]
      
      xreg_training_ts <- 
        ts(xreg_training_data,
           start = c(as.numeric(format(input$data_start_date, "%Y")), 
                     yday(as.Date(input$data_start_date, 
                                  format = "%Y/%m/%d"))),
           frequency = frequency)
      
      xreg_forecast_ts <- 
        ts(xreg_forecast_data,
           start = c(as.numeric(format(fore_start_date, "%Y")), 
                     yday(as.Date(fore_start_date, 
                                  format = "%Y/%m/%d"))),
           frequency = frequency)
      
      xreg_train <- data.matrix(xreg_training_ts)
      xreg_fore <- data.matrix(xreg_forecast_ts)
      
      auto_arimax_train <- auto.arima(training_ts,
                                      max.p = 8, max.q = 8, max.d = 2,
                                      method = "ML", approximation = FALSE,
                                      ic = input$select_IC, stepwise = FALSE,
                                      xreg = xreg_train)
      forecast <- forecast(auto_arimax_train, xreg = xreg_fore)
      acc_table <- accuracy(forecast)
      regr_coef <- coeftest(auto_arimax_train)
      output_list <- list(actual = working_data,
                          predicted = auto_arimax_train$fitted,
                          errors = auto_arimax_train$residuals,
                          p = auto_arimax_train$arma[1],
                          d = auto_arimax_train$arma[6],
                          q = auto_arimax_train$arma[2],
                          acc_table = acc_table,
                          regr_coef = regr_coef,
                          forecast = forecast)
    }
    return(output_list)
  })
  
  ### show arima p, d, and q parameters
  output$arima_model_des_fore <- 
    eventReactive(input$run_arima_arimax_button_fore, {
      paste("ARIMA(", auto_arima_fore()$p, ",", auto_arima_fore()$d, ",", 
            auto_arima_fore()$q, ") model generated")
    })
  
  ### Plot forecast vs actual for auto-arima / arimax 
  output$auto_arima_chart_fore <- renderPlot({
    plot(auto_arima_fore()$forecast)
    lines(auto_arima_fore()$actual, col = "black")
  })
  
  ### Show variable coefficients
  output$auto_arima_coef_fore <- renderTable(tidy(auto_arima_fore()$regr_coef))
  
  ### show prediction accuracies
  output$auto_arima_accuracies_fore <- renderTable(auto_arima_fore()$acc_table)
  
  
  
  
  
  
  ## Manual arima models
  observeEvent(input$choose_man_arima_arimax_fore, {
    if(input$choose_man_arima_arimax_fore == "man_arima") {
      updateSelectInput(session, "select_man_arimax_variable_fore", 
                        choices = "none")
    } else {updateSelectInput(session, "select_man_arimax_variable_fore", 
                              choices = colnames(data_table()))
    }})
  
  ### generate xreg matrix
  man_external_data_fore <- 
    eventReactive(input$update_man_ext_var_button_fore, {
    req(data_table())
    if(is.null(input$select_man_arimax_variable_fore)) data_table()
    else data_table()[, colnames(data_table()) %in% 
                        input$select_man_arimax_variable_fore]
  })
  
  ### Run manual arima / arimax
  man_arima_fore <- eventReactive(input$run_man_arima_arimax_button_fore, {
    req(selected_data())
    ## Sort data by frequency type
    if(input$ts_frequency == "day") {
      frequency <- 365
      period_1 <- days(1)
    } else if (input$ts_frequency == "week") {
      frequency <- 365/7
      period_1 <- weeks(1)
    } else if (input$ts_frequency == "month") {
      frequency <- 12
      period_1 <- months(1)
    } else if (input$ts_frequency == "quarter") {
      frequency <- 4
      period_1 <- months(3)
    } else {
      frequency <- 1
      period_1 <- years(1)
    }
    
    ## Slicing the dataset into training set and forecast set
    length_full <- nrow(selected_data())
    length_train <- length_full - input$forecast_period_man_ar
    
    training_data <- selected_data()[1:length_train,]
    forecast_data <- selected_data()[(length_train + period_1):length_full,]
    
    fore_start_date <- ymd(input$data_start_date) + days(length_train)
    
    working_data <- 
      ts(selected_data(),
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    training_ts <- 
      ts(training_data,
         start = c(as.numeric(format(input$data_start_date, "%Y")), 
                   yday(as.Date(input$data_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    forecast_ts <- 
      ts(forecast_data,
         start = c(as.numeric(format(fore_start_date, "%Y")), 
                   yday(as.Date(fore_start_date, 
                                format = "%Y/%m/%d"))),
         frequency = frequency)
    
    ## Running manual arima / arimax
    if(input$choose_man_arima_arimax_fore == "man_arima") {
      man_arima_train <- Arima(training_ts,
                               order = c(input$manual_p_parameter_fore,
                                         input$manual_d_parameter_fore,
                                         input$manual_q_parameter_fore))
      forecast <- forecast(man_arima_train, h = input$forecast_period_man_ar)
      acc_table <- accuracy(forecast)
      regr_coef <- coeftest(man_arima_train)
      output_list <- list(actual = working_data, 
                          predicted = man_arima_train$fitted,
                          errors = man_arima_train$residuals,
                          p = input$manual_p_parameter_fore,
                          d = input$manual_d_parameter_fore,
                          q = input$manual_q_parameter_fore,
                          acc_table = acc_table,
                          regr_coef = regr_coef,
                          forecast = forecast)
    } else {
      xreg_training_data <- man_external_data_fore()[1:length_train,]
      xreg_forecast_data <- 
        man_external_data_fore()[(length_train + period_1):length_full,]
      
      xreg_training_ts <- 
        ts(xreg_training_data,
           start = c(as.numeric(format(input$data_start_date, "%Y")), 
                     yday(as.Date(input$data_start_date, 
                                  format = "%Y/%m/%d"))),
           frequency = frequency)
      
      xreg_forecast_ts <- 
        ts(xreg_forecast_data,
           start = c(as.numeric(format(fore_start_date, "%Y")), 
                     yday(as.Date(fore_start_date, 
                                  format = "%Y/%m/%d"))),
           frequency = frequency)
      
      xreg_train <- data.matrix(xreg_training_ts)
      xreg_fore <- data.matrix(xreg_forecast_ts)
      
      man_arimax_train <- Arima(training_ts,
                                order = c(input$manual_p_parameter_fore,
                                          input$manual_d_parameter_fore,
                                          input$manual_q_parameter_fore),
                                xreg = xreg_train)
      forecast <- forecast(man_arimax_train, xreg = xreg_fore)
      acc_table <- accuracy(forecast)
      regr_coef <- coeftest(man_arimax_train)
      output_list <- list(actual = working_data,
                          predicted = man_arimax_train$fitted,
                          errors = man_arimax_train$residuals,
                          p = input$manual_p_parameter_fore,
                          d = input$manual_d_parameter_fore,
                          q = input$manual_q_parameter_fore,
                          acc_table = acc_table,
                          regr_coef = regr_coef,
                          forecast = forecast)
    }
    return(output_list)
  })
  
  ### Plot forecast vs actual for manual arima
  output$man_arima_chart_fore <- renderPlot({
    plot(man_arima_fore()$forecast)
    lines(man_arima_fore()$actual, col = "black")
  })
  
  ### show defined p, d, and q parameters
  output$man_arima_model_des_fore <- 
    eventReactive(input$run_man_arima_arimax_button_fore, {
      paste("ARIMA(", input$manual_p_parameter_fore, ",", 
            input$manual_d_parameter_fore, ",", 
            input$manual_q_parameter_fore, ") model")
    })
  
  ### Show variable coefficients
  output$man_arima_coef_fore <- renderTable(tidy(man_arima_fore()$regr_coef))
  
  ### show prediction accuracies
  output$man_arima_accuracies_fore <- renderTable(man_arima_fore()$acc_table)
  
  
  }

shinyApp(ui = ui, server = server)