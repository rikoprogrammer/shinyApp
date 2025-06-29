

server <- function(input, output, session) {
  
  input_dataset <- reactive(
    
    
    if(is.null(input$file)) {
      return("")
    }else {
      
      #allow for both csv and xlsx file uploads
      
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv  = readr::read_csv(file = input$file$datapath) |> janitor::clean_names(),
             xlsx = readxl::read_excel(path = input$file$datapath, sheet = input$sheet_index) |> 
               janitor::clean_names(),
             validate("Invalid file; Please upload a .csv or .xlsx file")
      )
    }
  ) 
  
  input_dataset2 <- reactive(
    
    
    if(is.null(input$file2)) {
      return("")
    }else {
      
      #allow for both csv and xlsx file uploads
      
      ext2 <- tools::file_ext(input$file2$name)
      switch(ext2,
             csv  = readr::read_csv(file2 = input$file2$datapath) |> janitor::clean_names(),
             xlsx = readxl::read_excel(path = input$file2$datapath, sheet = input$sheet_index) |> 
               janitor::clean_names(),
             validate("Invalid file; Please upload a .csv or .xlsx file")
      )
    }
  ) 
  
  
  vars2 <- tibble::tribble(
    
    ~id, ~ label,
    "y_var", "select your dependent variable",
    "x_vars", "select predictor variables",
    "tr_vars", "select variables to transform",
    "endo_vars", "endogenous variables",
    "exo_vars", "exogenous variables",
    "iv_vars", "instrument variables",
    "xa","choose some independent variables"
  )
  
  updatefun <- function(id, label, choices = colnames(input_dataset())) {
    updateSelectInput(session, id, label, choices = choices)
  }
  
  #observe file being selected
  observeEvent(input$file, {
    # updateSelectInput(session,
    #                   inputId = "y_var",
    #                   label = "select a continuous dependent variable",
    #                   choices = colnames(input_dataset()))
    
    pmap(vars2, updatefun)
    
    # updateSelectInput(session,
    #                   inputId = "x_vars",
    #                   label = "select predictor variables",
    #                   choices = colnames(input_dataset()))
    
    updateSelectInput(session,
                      inputId = "x_vars2",
                      label = "select a single predictor",
                      choices = colnames(input_dataset()))
    
    # updateSelectInput(session,
    #                   inputId = "tr_vars",
    #                   label = "select variables to transform",
    #                   choices = colnames(input_dataset()))
    
    
    updateSelectInput(session,
                      inputId = "series",
                      label = "choose a series to interpolate",
                      choices = colnames(input_dataset2()))
  })
  
  
  #observe file being selected
  observeEvent(input$file2, {
    
    updateSelectInput(session,
                      inputId = "series",
                      label = "choose a series to interpolate",
                      choices = colnames(input_dataset2()))
  })
  
  
  # create a data frame for doing ML/Classification models
  # these models require the dependent varibale to be coded as 0/1 or if it is
  # in character format then it should be coded as a factor variable
  
  dat <- reactive({
    
    input_dataset() |> 
    dplyr::select(
      dependent_var = input$y_var,
      input$x_vars) |> 
    mutate_if(is.character, as.factor) |> 
   drop_na()
  })
  

################### INTRODUCE THE SECOND APP CODE ###############
####### N/B: Initially we had two apps; one for linear regression models and time series models
######## and another one for IV regression and machine learning models ###########
  
  
  ## Create training  and testing sets based on the data set provided by the user
  
  train <- reactive({
    set.seed(123)
    
    if(is.null(input$file) | input$prop < 0){
      return('Please upload a file and specify the portion for the training set')
    }else if(!is.null(input$file) & input$prop > 0){
      
      
      data_split <- initial_split(dat(), prop = input$prop)
      
      train <- training(data_split)
    }
   
  })
  
  
  test <- reactive({
    set.seed(123)
 
    data_split <- initial_split(dat(), prop = input$prop)
    
    test <- testing(data_split)
  })
  
  # Logistic regression/classification model 
  
  logistic_fit <- reactive({
    
    if(is.null(input$file) & input$idn0 < 0) {
      return("Please select your data first and run the model!")
    }else if(!is.null(input$file) & input$idn0 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      # check <- get(input$y_var, input_dataset())
      # if (!is.numeric(check)) {
      #   validate(paste0("'", input$y_var, 
      #                   "' is not a numeric column, please select a numeric column "))
      # }
      # check
      
      set.seed(123)
      
      logistic_spec <- logistic_reg() %>%
        set_engine("glm") %>%
        set_mode("classification")
      
      
      # Fit the model to the training data
      logistic_fit <- logistic_spec %>%
        fit(dependent_var ~ ., data = train())
      
    }
    
  } )
  
  
  
  #Decision tree regression/classification
  
  tree_fit <- reactive({
    
    if(is.null(input$file) & input$idn1 < 0) {
      return("Please select your data first and run the model!")
    }else if(!is.null(input$file) & input$idn1 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      # check <- get(input$y_var, input_dataset())
      # if (!is.numeric(check)) {
      #   validate(paste0("'", input$y_var, 
      #                   "' is not a numeric column, please select a numeric column "))
      # }
      # check
      
      set.seed(123)
      
      tree_spec <- decision_tree() %>%
        set_engine("rpart") %>%
        set_mode("classification")
      
      # Fit the model to the training data
      tree_fit <- tree_spec %>%
        fit(dependent_var ~ ., data = train())
      
    }
    
  } )
  
  
  ### Random forest regression
  
  forest_fit <- reactive({
    
    if(input$idn2 > 0){
      
      rf_defaults <- rand_forest(trees = 200, min_n = 5, 
                                 mode = "classification")
      
      set.seed(123)
      
      forest_fit <- 
        rf_defaults %>%
        set_engine("ranger") %>%
        fit(
          dependent_var ~ ., data = train()
        )
    }
  })
  
  
  ### Support vector machine model
  
  svm_fit <- reactive({
    
    if(input$idn33 > 0){
      # svm_fit = svm(formula = dependent_var ~ .,
      #               data = train(),
      #               type = 'eps-regression',
      #               kernel = 'radial')
      
      svm_spec <- svm_rbf() %>%
        set_mode("classification") %>%
        set_engine("kernlab", scaled = FALSE)
      
      svm_fit <- svm_spec %>%
        fit(dependent_var ~ ., data = train())
    }
  }
  )
  
  
  
  #############
  
  #Instrument variable regression
  
  ###########
  
  data_iv <- reactive(
    input_dataset() |> 
      dplyr::select(dependent_var = input$y_var,
                    input$exo_vars,
                    input$endo_vars,
                    input$iv_vars)
  )
  
  form <- reactive({
    
    if(!is.null(input$file) & input$idn3 > 0){
      exo  = paste0(input$exo_vars)
      endo = paste0(input$endo_vars)
      iv   = paste0(input$iv_vars)
      
      as.formula(paste('dependent_var ~ ', paste(paste(endo, collapse = "+"),"+",
                                                 paste(exo, collapse = "+")),
                       paste0("|", paste(exo, collapse = "+"), "+",
                              paste(iv, collapse = "+"))))
    }
    
  })
  
  iv_fit <- reactive({
    
    if(!is.null(input$file) & input$idn3 > 0){
      iv_fit <- ivreg(form(), data = data_iv())
    }
    
  })
  

  
  
  ##Coefficients of the IV model
  
  coefs_iv <- reactive({
    
    if(!is.null(input$file) & input$idn3 > 0) {
      coefs <- summary(iv_fit())$coefficients
      coefs_iv <- as.data.frame(coefs)
    }
  })
  
  
  #Prediction data sets for machine learning models
  
  preds0_df <- reactive(
    
    if(input$idn0 > 0){
      
      preds0_df = test() %>%
        dplyr::select(dependent_var) %>%
        as.data.frame() |> 
        dplyr::bind_cols(
          stats::predict(logistic_fit(), new_data = test(), type = "class"),
          stats::predict(logistic_fit(), new_data = test(), type = "prob")
          
        )
      
    }
    
  )
  
  preds1_df <- reactive(
    
    if(input$idn1 > 0){
      
      
      preds1_df = test() %>%
        dplyr::select(dependent_var) %>%
        as.data.frame() |> 
        dplyr::bind_cols(
          stats::predict(tree_fit(), new_data = test())
        )
      
    }
  
  )
  

  
  preds2_df <- reactive(
    
    if(input$idn2 > 0){
      
      
      preds2_df =  test() %>%
        dplyr::select(dependent_var) %>%
        as.data.frame() |> 
        dplyr::bind_cols(
          stats::predict(forest_fit(), new_data = test())
        )
    }
    
  )
  
  preds3_df <- reactive({
    if(input$idn33 > 0){
      
      predictions <- svm_fit() %>%
        stats::predict(test())
      
      preds3_df = test() %>%
        dplyr::select(dependent_var) %>%
        as.data.frame() |> 
        dplyr::bind_cols(
          predictions
        )
    }
  })
  
  
  output$metrics0 <- renderPrint({
    
    if(input$idn0 > 0) {
      
      cat("RESULTS FOR LOGISTIC CLASSIFICATION \n")
      cat("\n")
      
      cat("A table for the log odds\n")
      
  
      broom::tidy(logistic_fit(), exponentiate = TRUE) |>
        knitr::kable(digits = 3) |>
        print()
      
      pred_class <- predict(logistic_fit(),
                            new_data = test(),
                            type = "class")
      
      # Prediction Probabilities
      pred_proba <- predict(logistic_fit(),
                            new_data = test(),
                            type = "prob")
      
      #final data preparation for model evaluation
      
      results <- test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(pred_class, pred_proba)
      
      
      # custom metrics
      custom_metrics <- metric_set(accuracy, sensitivity, specificity,
                                   precision, recall, f_meas, kap, mcc)
      
      cat("\n")
      cat("\n")
      cat("Classification Metrics\n")
   
      
      custom_metrics(results,
                     truth    = dependent_var,
                     estimate = .pred_class) |>
        knitr::kable()
      
    }
    
  })
  
  
  output$metrics1 <- renderPrint({
    
    if(input$idn1 > 0) {
      
      cat("RESULTS FOR DECISION TREE REGRESSION \n")
      
      # Make predictions on the testing data
      # predictions <- tree_fit() %>%
      #   stats::predict(test()) %>%
      #   pull(.pred)
      # 
      # #Calculate RMSE and R-squared
      # 
      # metrics <- metric_set(rmse, rsq)
      # model_performance <- test() %>%
      #   dplyr::mutate(predictions = predictions) %>%
      #   metrics(truth = dependent_var, estimate = predictions)
      # 
      # model_performance |> 
      #   pander::pander()
      # 
      
      # May 31 2025 changed from decision tree regression to decision tree classification
      
      
      pred_class <- predict(tree_fit(),
                            new_data = test(),
                            type = "class")
      
      # Prediction Probabilities
      pred_proba <- predict(tree_fit(),
                            new_data = test(),
                            type = "prob")
      
      #final data preparation for model evaluation
      
      results <- test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(pred_class, pred_proba)
      
      
      # custom metrics
      custom_metrics <- metric_set(accuracy, sensitivity, specificity,
                                   precision, recall, f_meas, kap, mcc)
      
      cat("\n")
      cat("\n")
      cat("Classification Metrics\n")
      
      
      custom_metrics(results,
                     truth    = dependent_var,
                     estimate = .pred_class) |>
        knitr::kable()
      
    }
    
  })
  
  
  output$metrics2 <- renderPrint({
    
    if(input$idn2 > 0){
      
      cat("RESULTS FOR RANDOM FOREST REGRESSION \n")
      
      # test_results <- 
      #   test() %>%
      #   dplyr::select(dependent_var) %>%
      #   bind_cols(
      #     stats::predict(forest_fit(), new_data = test())
      #   )
      # 
      # # summarize performance
      # test_results |> metrics(truth = dependent_var, 
      #                          estimate = .pred) |> 
      #   pander::pander()
      
      
      pred_class <- predict(forest_fit(),
                            new_data = test(),
                            type = "class")
      
      # Prediction Probabilities
      pred_proba <- predict(forest_fit(),
                            new_data = test(),
                            type = "prob")
      
      #final data preparation for model evaluation
      
      results <- test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(pred_class, pred_proba)
      
      
      # custom metrics
      custom_metrics <- metric_set(accuracy, sensitivity, specificity,
                                   precision, recall, f_meas, kap, mcc)
      
      cat("\n")
      cat("\n")
      cat("Classification Metrics\n")
      
      
      custom_metrics(results,
                     truth    = dependent_var,
                     estimate = .pred_class) |>
        knitr::kable()
    }
  })
  
  
 
  
  output$metrics33 <- renderPrint({
    
    if(input$idn33 > 0){
      
      cat("RESULTS FOR SVM MODEL \n")
      
      # # Make predictions on the testing data
      # predictions <- svm_fit() %>%
      #   predict(test())
      # 
      # #Calculate RMSE and R-squared
      # metrics <- metric_set(rmse, rsq)
      # model_performance <- test() %>%
      #   dplyr::mutate(predictions = predictions) %>%
      #   metrics(truth = dependent_var, estimate = predictions)
      # 
      # model_performance |> 
      #   pander::pander()
      
      pred_class <- predict(svm_fit(),
                            new_data = test(),
                            type = "class")
      
      # Prediction Probabilities
      pred_proba <- predict(svm_fit(),
                            new_data = test(),
                            type = "prob")
      
      #final data preparation for model evaluation
      
      results <- test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(pred_class, pred_proba)
      
      
      # custom metrics
      custom_metrics <- metric_set(accuracy, sensitivity, specificity,
                                   precision, recall, f_meas, kap, mcc)
      
      cat("\n")
      cat("\n")
      cat("Classification Metrics\n")
      
      
      custom_metrics(results,
                     truth    = dependent_var,
                     estimate = .pred_class) |>
        knitr::kable()
    }
    
  })
  
  
  
  output$iv_summary <- renderPrint({
    
    if(input$idn3 > 0){
      
      print(form())
      summary(iv_fit(), vcov = sandwich, diagnostics = TRUE)
    }
  })
  

  # SIMPLEX regression implementation - 28th Feb 2025
  
  
  simp_df <- reactive(
    
     input_dataset() |> 
       dplyr::mutate(date =  input_dataset()[[1]]) |> 
       dplyr::select(
         date,
         input$x_vars,
         dep = input$y_var
       ) |> 
       drop_na()
  )
  
  simp_df_indep <- reactive(

    input_dataset() |>
      dplyr::select(
         input$x_vars
      ) |> 
      drop_na()
  )
  
 
  
  output$simp_summary <- renderPrint({
    
    if(input$ids3 > 0) {
      
      
      X <<- as.matrix(simp_df_indep())
      Y <<- as.matrix(simp_df()$dep)
      
      
      n <- dim(X)[2]
      XX <<- t(X) %*% X
      XY <<- t(X) %*% Y
      # Q is (X'X)^-1
      Q <- solve(XX)
      # P is the Moore Pensrose Pseudo inverse of X
      P <<- Q %*% t(X)
      

      # print(P)
      objVal <- c()

      workProbs <- seq(0.2, 0.3, 0.01)

      for (workProb in workProbs) {
        theta <- runGD(workProb)
        obval <- SSE(X, Y, theta)
        objVal <- c(objVal, obval)
        cat("workProb =", workProb, "Objective value =", round(obval, 5), "\n")
      }

      workProb <- workProbs[which.min(objVal)]
      thetaBest <- runGD(workProb)

      print('Best parameters: ')
      print(thetaBest)

      pred <<- X %*% thetaBest

    }
    
  })
  
  
  output$simp_plot <- renderPlot({
    
    if(input$ids3 > 0){
      
      x = as.Date(simp_df()$date)
      
      plot(x, pred, xlab = "Date", ylab = input$y_var, 
           main = "Forecast vs Target", col = "blue", type = "l", 
           lty = 1, lwd = 2, ylim = range(c(pred, simp_df()$dep)), 
           las = 2, cex.axis = 0.8, cex.lab = 0.8)
      lines(x, simp_df()$dep, col = "red", lty = 2, lwd = 2)
      legend("topleft", c('Forecast', "Target"), lty = c(1,2), lwd = c(2,2), col = c("blue", "red")) 
    }
    
  })
  
  
  
  # CONSTRAINED regression implementation
  
  # The imput is a matrix Y of nx1 and a matrix X of nxk , the only condition is both X and Y must 
  # have the same number of rows and that Y is a column vector 
  
  # the result is a vector of betas of kx1 that they summ up to 1 
  
  
  # The function convertor() is defined in the helper_functions.R script
  
  constrained_df <- reactive({
    
    req(input_dataset())
    
    constrained_df <- input_dataset() |> 
      dplyr::select(
        dep = input$y_var
      )
  }
  )
  
  constrained_df2 <- reactive({
    
    req(input_dataset())
    
    constrained_df <- input_dataset() |> 
      dplyr::select(
        dep   = input$y_var,
        indep = input$xa
      )
  }
  )
  
  constrained_df_indep <- reactive({
    
    req(input_dataset())
    
    constrained_df_indep <- input_dataset() |> 
      dplyr::select(
        input$xa
      )
  }
  )
  
  
  output$cons_summary <- renderPrint({
    
    if(input$idc3 > 0){
      
      # this is executed for at least two independent variables
      
      if(ncol(constrained_df_indep()) > 1 ) {
        results = convertor(X = matrix(c(constrained_df_indep(), recursive = TRUE,
                                         use.names = FALSE), ncol = ncol(constrained_df_indep())),
                            
                            Y = matrix(constrained_df()$dep, ncol = 1))
      }else {
        
        # 8th Jan 25: This branch is executed for a single independent variable,
        # however some data points are lost in the process, eg am creating 
        # a 4 by 4 matrix from the single column which has more than 16 values.
        # This should be taken as a temporary solution, but in future it can
        # be improved.
        
        results = convertor(X = matrix(constrained_df2()$indep,  nrow = 4, ncol = 4),
                            Y = matrix(constrained_df2()$dep,    nrow = 4, ncol = 1))
      }
      
      

      cat("Constrained matrix \n")
      print(results)

      cat("Confirm if the coefficients sum to 1 \n")
      print(sum(results))
    }
  })

  
  # 8th of Feb 2025: Constrained regression with zero intercept
  
  
  output$cons_summary_ <- renderPrint({
    

      # this is executed for at least two independent variables
      
      if(ncol(constrained_df_indep()) > 1 ) {
        results = convertor_no(X = matrix(c(constrained_df_indep(), recursive = TRUE,
                                         use.names = FALSE), ncol = ncol(constrained_df_indep())),
                            
                               Y = matrix(constrained_df()$dep, ncol = 1))
      }else {
        
        
        results = convertor_no(X = matrix(constrained_df2()$indep,  nrow = 4, ncol = 4),
                               Y = matrix(constrained_df2()$dep,    nrow = 4, ncol = 1))
      }
      
      
      
      cat("Constrained matrix \n")
      print(results)
      
      cat("Confirm if the coefficients sum to 1 \n")
      print(sum(results))

  })
  
  
  ### KALMAN-FILTER IMPLEMENTATION - 03rd Mar 2025
  
  ## Kalman Filter with constraint
  
  
  kalman_df <- reactive(
    
    #req(input_dataset()),
    
    input_dataset() |> 
      dplyr::select(
        dep   = input$y_var,
        indep = input$x_vars[1]
      )
  )
  
  output$kalman_summary <- renderPrint({
    
    if(input$id_kalman > 0){
      
      y1 <- kalman_df()$dep
      y1 <- na.omit(y1)
      x1 <- kalman_df()$indep
      
      
      
      # Time step
      delta_t <- 1
      
      # State transition matrix F
      F <- matrix(c(1, delta_t, 0, 1), nrow = 2)
      
      # Measurement matrix H
      H <- matrix(c(1, 0), nrow = 1)
      
      # Process noise covariance Q
      Q <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
      
      # Measurement noise covariance R
      R <- matrix(1, nrow = 1)
      
      # Initial state estimate
      x_hat <- matrix(c(13000, 13000), nrow = 2)
      
      # Initial error covariance
      P <- matrix(c(1, 0, 0, 1), nrow = 2)
      
      # Number of time steps
      n_steps <- 98
      
      
      
      # Simulated true position
      true_position <- x1
      
      # Simulate noisy measurements
      set.seed(123)
      measurements <- y1
      
      # Storage for estimates
      x_hat_estimates <- matrix(NA, nrow = 2, ncol = n_steps)
      
      # Kalman Filter loop
      for (k in 1:n_steps) {
        # Predict
        x_hat <- F %*% x_hat
        P <- F %*% P %*% t(F) + Q
        
        # Update
        y <- measurements[k] - H %*% x_hat
        S <- H %*% P %*% t(H) + R
        K <- P %*% t(H) %*% solve(S)
        
        x_hat <- x_hat + K %*% y
        P <- (diag(2) - K %*% H) %*% P
        
        # Store the estimates
        x_hat_estimates[, k] <- x_hat
      }
      
      
      
      a <-  x_hat_estimates[1, ]
      prediction <-a[-length(a)]
      
    
      
      # Create time series objects
      x1 <- measurements
      
      x2 <- a
      
      #print estimates and prediction
      
      # cat('Estimates')
      # print(x_hat_estimates)
      
      cat("Predictions \n")
      print(prediction)
      
    }
    
  })
  
  output$kalman_plot <- renderPlot({
    
    if(input$id_kalman > 0){
      
      y1 <- kalman_df()$dep
      y1 <- na.omit(y1)
      x1 <- kalman_df()$indep
      
      
      
      # Time step
      delta_t <- 1
      
      # State transition matrix F
      F <- matrix(c(1, delta_t, 0, 1), nrow = 2)
      
      # Measurement matrix H
      H <- matrix(c(1, 0), nrow = 1)
      
      # Process noise covariance Q
      Q <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
      
      # Measurement noise covariance R
      R <- matrix(1, nrow = 1)
      
      # Initial state estimate
      x_hat <- matrix(c(13000, 13000), nrow = 2)
      
      # Initial error covariance
      P <- matrix(c(1, 0, 0, 1), nrow = 2)
      
      # Number of time steps
      n_steps <- 98
      
      
      
      # Simulated true position
      true_position <- x1
      
      # Simulate noisy measurements
      set.seed(123)
      measurements <- y1
      
      # Storage for estimates
      x_hat_estimates <- matrix(NA, nrow = 2, ncol = n_steps)
      
      # Kalman Filter loop
      for (k in 1:n_steps) {
        # Predict
        x_hat <- F %*% x_hat
        P <- F %*% P %*% t(F) + Q
        
        # Update
        y <- measurements[k] - H %*% x_hat
        S <- H %*% P %*% t(H) + R
        K <- P %*% t(H) %*% solve(S)
        
        x_hat <- x_hat + K %*% y
        P <- (diag(2) - K %*% H) %*% P
        
        # Store the estimates
        x_hat_estimates[, k] <- x_hat
      }
      
      
      
      a <-  x_hat_estimates[1, ]
      prediction <-a[-length(a)]

      
      # Create time series objects
      x1 <- measurements
      
      x2 <- a
      
      # Determine the range of the y-axis to accommodate both time series
      y_range <- range(c(x1, x2), na.rm = TRUE)
      
      # Plot the first time series with adjusted y-axis limits
      plot(x1, type = "l", col = "blue", lwd = 2, ylab = "Values", xlab = "Time", 
           main = "", ylim = y_range)
      
      # Add the second time series to the same plot
      lines(x2, col = "red", lwd = 2)
      
      # Add a legend to differentiate between the two time series
      legend("topright", legend = c("x1", "x2"), col = c("blue", "red"), lty = 1, lwd = 2)
      
    }
    
  })
  
  
  
  ## Place-holder code for Kalman Filter without the constraint - this maybe modified at a latter date
  
  output$kalman_summary0 <- renderPrint({
    
    if(input$id_kalman > 0){
      
      y1 <- kalman_df()$dep
      y1 <- na.omit(y1)
      x1 <- kalman_df()$indep
      
      
      
      # Time step
      delta_t <- 1
      
      # State transition matrix F
      F <- matrix(c(1, delta_t, 0, 1), nrow = 2)
      
      # Measurement matrix H
      H <- matrix(c(1, 0), nrow = 1)
      
      # Process noise covariance Q
      Q <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
      
      # Measurement noise covariance R
      R <- matrix(1, nrow = 1)
      
      # Initial state estimate
      x_hat <- matrix(c(13000, 13000), nrow = 2)
      
      # Initial error covariance
      P <- matrix(c(1, 0, 0, 1), nrow = 2)
      
      # Number of time steps
      n_steps <- 98
      
      
      
      # Simulated true position
      true_position <- x1
      
      # Simulate noisy measurements
      set.seed(123)
      measurements <- y1
      
      # Storage for estimates
      x_hat_estimates <- matrix(NA, nrow = 2, ncol = n_steps)
      
      # Kalman Filter loop
      for (k in 1:n_steps) {
        # Predict
        x_hat <- F %*% x_hat
        P <- F %*% P %*% t(F) + Q
        
        # Update
        y <- measurements[k] - H %*% x_hat
        S <- H %*% P %*% t(H) + R
        K <- P %*% t(H) %*% solve(S)
        
        x_hat <- x_hat + K %*% y
        P <- (diag(2) - K %*% H) %*% P
        
        # Store the estimates
        x_hat_estimates[, k] <- x_hat
      }
      
      
      
      a <-  x_hat_estimates[1, ]
      prediction <-a[-length(a)]
      
      
      
      # Create time series objects
      x1 <- measurements
      
      x2 <- a
      
      #print estimates and prediction
      
      # cat('Estimates')
      # print(x_hat_estimates)
      
      cat("Predictions \n")
      print(prediction)
      
    }
    
  })
  
  output$kalman_plot0 <- renderPlot({
    
    if(input$id_kalman > 0){
      
      y1 <- kalman_df()$dep
      y1 <- na.omit(y1)
      x1 <- kalman_df()$indep
      
      
      
      # Time step
      delta_t <- 1
      
      # State transition matrix F
      F <- matrix(c(1, delta_t, 0, 1), nrow = 2)
      
      # Measurement matrix H
      H <- matrix(c(1, 0), nrow = 1)
      
      # Process noise covariance Q
      Q <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
      
      # Measurement noise covariance R
      R <- matrix(1, nrow = 1)
      
      # Initial state estimate
      x_hat <- matrix(c(13000, 13000), nrow = 2)
      
      # Initial error covariance
      P <- matrix(c(1, 0, 0, 1), nrow = 2)
      
      # Number of time steps
      n_steps <- 98
      
      
      
      # Simulated true position
      true_position <- x1
      
      # Simulate noisy measurements
      set.seed(123)
      measurements <- y1
      
      # Storage for estimates
      x_hat_estimates <- matrix(NA, nrow = 2, ncol = n_steps)
      
      # Kalman Filter loop
      for (k in 1:n_steps) {
        # Predict
        x_hat <- F %*% x_hat
        P <- F %*% P %*% t(F) + Q
        
        # Update
        y <- measurements[k] - H %*% x_hat
        S <- H %*% P %*% t(H) + R
        K <- P %*% t(H) %*% solve(S)
        
        x_hat <- x_hat + K %*% y
        P <- (diag(2) - K %*% H) %*% P
        
        # Store the estimates
        x_hat_estimates[, k] <- x_hat
      }
      
      
      
      a <-  x_hat_estimates[1, ]
      prediction <-a[-length(a)]
      
      
      # Create time series objects
      x1 <- measurements
      
      x2 <- a
      
      # Determine the range of the y-axis to accommodate both time series
      y_range <- range(c(x1, x2), na.rm = TRUE)
      
      # Plot the first time series with adjusted y-axis limits
      plot(x1, type = "l", col = "blue", lwd = 2, ylab = "Values", xlab = "Time", 
           main = "", ylim = y_range)
      
      # Add the second time series to the same plot
      lines(x2, col = "red", lwd = 2)
      
      # Add a legend to differentiate between the two time series
      legend("topright", legend = c("x1", "x2"), col = c("blue", "red"), lty = 1, lwd = 2)
      
    }
    
  })
  
  
  
  ### SOLVER IMPLEMENTATION - not yet implemented
  
  
  
  
  # Render results from machine learning models
  
  output$preds0 <-  renderDT({
    
    if(input$idn0 > 0){
      test_results <- 
        test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(
          predict(logistic_fit(), new_data = test())
        )
      
      test_results 
    }
  })
  
  output$preds1 <-  renderDT({
    
    if(input$idn1 > 0){
      test_results <- 
        test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(
          predict(tree_fit(), new_data = test())
        )
      
      test_results 
    }
  })
  
  output$preds2 <-  renderDT({
    
    if(input$idn2 > 0){
      test_results <- 
        test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(
          stats::predict(forest_fit(), new_data = test())
        )
      
      test_results 
    }
    
  })
  
  output$preds3 <-  renderDT({
    
    if(input$idn33 > 0){
      
      predictions <- svm_fit() %>%
        predict(test())
      
      test_results <- 
        test() %>%
        dplyr::select(dependent_var) %>%
        bind_cols(
          predictions
        )
      
      test_results 
    }
  })
  

  
  #download a report in pdf format: am only downloading reports related to model 1 and model 2,
  #but other model reports can as well be included in future releases of the app.
  
  
  output$dw5_ <- downloadHandler(
    filename = function() {
      paste0("model-report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      rmarkdown::render("reports/report.Rmd",
                        output_file = file,
                        params = list(
                          title = "Model Report",
                          mod1 = fit1(),
                          mod2 = fit2(),
                          mod3 = fit_ridge(),
                          mod4 = forward_fit(),
                          mod5 = backward_fit(),
                          plot = ts_plot()
                          
                        ),
                        envir = new.env(),
                        intermediates_dir = tempdir())
    }
  )
  
###### END OF THE SECOND APP: IV regression and machine learning models ###
 

  #### Rendering the raw data, imputed data, and the log transformed data sets
  
  output$raw_data <- renderDT({
    req(input_dataset())
    
    if(is.null(input$file) & input$raw_data1 < 0) {
      return("")
    }else{
      input_dataset()
    }
    
  })
  
  output$imp_data <- renderDT({
    req(input_dataset())
    
    if(is.null(input$file) & input$imp_data1 < 0) {
      return("")
    }else{
      imp_data()
    }
    
  })
  
  
  
  output$log_data <- renderDT(
    
    if(is.null(input$file) & input$l_transform < 0) {
      
      return("")
    } else if(!is.null(input$file) & input$l_transform > 0) {
      
      log_data()
    }
  )
  
  ### First multiple linear regression: 
  
  fit1 <- reactive({
    
    if(input$run1 < 0) {
      return("Please select your data first and choose proportion for training set!")
    }else if(!is.null(input$file) & input$run1 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      check <- get(input$y_var, input_dataset())
      if (!is.numeric(check)) {
        validate(paste0("'", input$y_var, 
                        "' is not a numeric column, please select a numeric column "))
      }
      check
      
      fit1 <- lm(dependent_var ~ ., data = dat())
    }
    
  } )
  
  
  #Corrections for autocorrelation based on cochrane orcutt
  
  fit_coch <- reactive(
    
    fit1() |> 
      cochrane.orcutt(convergence = 5, max.iter = 1000)
  )
  
  
  # Make a function that does orcutt transformations, i will use this to 
  # transform all columns in one pass, thanks to purrr map_df function
  
  fun1 <- function(x) {
    
    x = x[-1] - x[-12]*fit_coch()$rho
  }
  
  
 
  dat_orcut <- reactive({
    
    input_dataset() |> 
      dplyr::select(
        dependent_var = input$y_var,
        input$x_vars ) |> 
      map_df(fun1) |> 
      mutate_if(is.character, as.factor) |>  
      drop_na()
  })
  

  dat_diff <- reactive({
    
    input_dataset() |> 
      dplyr::select(
        dependent_var = input$y_var,
        input$x_vars ) |> 
      mutate_if(is.character, as.factor) |>  
      drop_na() |> 
      as.matrix() |> 
      diff() |> 
      as.data.frame()
    
  })
  
  
  fit1_corrected <- reactive({
    
    if(input$run1 < 0) {
      return("Please select your data first and choose proportion for training set!")
    }else if(!is.null(input$file) & input$run1 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      check <- get(input$y_var, input_dataset())
      if (!is.numeric(check)) {
        validate(paste0("'", input$y_var, 
                        "' is not a numeric column, please select a numeric column "))
      }
      check
      
      fit1_corrected <- lm(dependent_var ~ ., data = dat_orcut())
    }
    
  } )
  
  #correction for autocorrelation bu using lagged dependent and independent
  # variable
  
  fit1_corrected_d <- reactive({
    
    if(input$run1 < 0) {
      return("Please select your data first and choose proportion for training set!")
    }else if(!is.null(input$file) & input$run1 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      check <- get(input$y_var, input_dataset())
      if (!is.numeric(check)) {
        validate(paste0("'", input$y_var, 
                        "' is not a numeric column, please select a numeric column "))
      }
      check
      
      fit1_corrected_d <- lm(dependent_var ~ ., data = dat_diff())
    }
  })
  
  
  
  fit2 <- reactive({
    
    if(is.null(input$file) & input$run2 < 0){
      return("Please select your data and run the model first!")
    }else if(!is.null(input$file) & input$run2 > 0 & input$run1 > 0){
      std_error = summary(fit1())$sigma
      
      dat3 = dat() %>%
        dplyr::mutate(residuals = summary(fit1())$residuals,
                      dummy1    = if_else(residuals >= std_error, 1, 0),
                      dummy2    = if_else(residuals < -std_error, 1, 0)) |> 
        dplyr::select(-residuals) 
      
      
      
      fit2 <- lm(dependent_var ~ ., data = dat3)
    }
    
  } )
  
  fit2_corrected <- reactive({
    
    if(input$run2 < 0) {
      return("Please select your data first and choose proportion for training set!")
    }else if(!is.null(input$file) & input$run2 > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      check <- get(input$y_var, input_dataset())
      if (!is.numeric(check)) {
        validate(paste0("'", input$y_var, 
                        "' is not a numeric column, please select a numeric column "))
      }
      check
      
      fit2_corrected <- lm(dependent_var ~ ., data = dat_orcut())
    }
    
  } )
  
  #RIDGE REGRESSION
  
  data_ridge <- reactive(
    
    input_dataset() |> 
      dplyr::select(
        dependent_var = input$y_var,
        input$x_vars) |> 
      drop_na() 
  )
  
  
  fit_ridge <- reactive({
    
    if(is.null(input$file) & input$run3 < 0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input) & input$run3 > 0){
      
      y = data_ridge()$dependent_var
      
      x = data_ridge() |> 
        dplyr::select(input$x_vars) |> 
        data.matrix()
      
      
      cv_model <- cv.glmnet(x, y, alpha = 0)
      
      best_lambda <- cv_model$lambda.min
      
      
      fit_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
    }
  })
  
  # Ridge regression without the intercept
  
  fit_ridge_zero <- reactive({
    
    if(is.null(input$file) & input$run3 < 0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input) & input$run3 > 0){
      
      y = data_ridge()$dependent_var
      
      x = data_ridge() |> 
        dplyr::select(input$x_vars) |> 
        data.matrix()
      
      
      cv_model <- cv.glmnet(x, y, alpha = 0)
      
      best_lambda <- cv_model$lambda.min
      
      
      fit_ridge_zero <- glmnet(x, y, alpha = 0, lambda = best_lambda,
                               intercept = FALSE)
    }
  })
  
  
  #Backward and forward step AIC models
  
  forward_fit <- reactive({
    
    if(is.null(input$file) & input$run4 <0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input$file) & input$run4 > 0){
      
      
      intercept_only = lm(dependent_var ~ 1, data = dat())

      model_all = lm(dependent_var ~ ., data = dat())

      forward_fit = stats::step(intercept_only, direction='forward', scope=formula(model_all),
                         trace=0)
    }
  }
  )
  
  backward_fit <- reactive({
    
    if(is.null(input$file) & input$run5 < 0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input$file) & input$run5 > 0) {
     
      
      full_mod = lm(dependent_var ~., data = dat())
      backward_fit = MASS::stepAIC(full_mod, direction = "backward", trace = FALSE)
    }
  }
  )
  
  # Zero intercept model
  
  fit_zero <- reactive({
    
    if(input$run_z < 0) {
      return("Please select your data first and choose proportion for training set!")
    }else if(!is.null(input$file) & input$run_z > 0){
      
      
      #Ensure that the user is selecting the correct dependent variable
      
      check <- get(input$y_var, input_dataset())
      if (!is.numeric(check)) {
        validate(paste0("'", input$y_var, 
                        "' is not a numeric column, please select a numeric column "))
      }
      check
      
      fit_zero <- lm(dependent_var ~ 0 + ., data = dat())
    }
    
  } )
  
  # imputed dataset
  
  # === Perform KNN Imputation === 2nd June 2025
  # Only apply KNN to numeric columns
  
  numeric_cols <- 
    reactive(
      
      input_dataset() |> 
        dplyr::select(where(is.numeric))
      
    )
  
  non_numeric_cols <- 
    reactive(
      
      input_dataset() |> 
        dplyr::select(where(Negate(is.numeric)))
    )
  
  
  
  
  imputed_numeric <- 
    reactive({
      
      if (ncol(numeric_cols()) == 0) stop("No numeric columns found to impute.")
      
      numeric_cols() |> 
        VIM::kNN(k = 5) |> # fix the number of neighbors to five for now - maybe we will change this to allow the user to specify in the next release of the app
        dplyr::select(is.numeric)
    })
  

  
  imp_data <- 
    reactive(
      
      non_numeric_cols() |> 
        bind_cols(imputed_numeric())
    )
    
  
  
  #Transformed data sets
  
  tr_data <- reactive(
    
    input_dataset() |> 
      dplyr::select(input$tr_vars) |> 
      drop_na()
  )
  
  log_data <- reactive({
    
   tr_data() |> 
      dplyr::select_if(where(is.numeric)) |> 
      log()
  })
  
  first_diff_data <- reactive({
    tr_data() |> 
      dplyr::select_if(where(is.numeric)) |> 
      as.matrix() |> 
      diff()
  })
  
  ts_lag1 <- reactive({
    
    tr_data() %>%
      dplyr::select_if(where(is.numeric)) |> 
      dplyr::lag()
  })
  
  ts_lag2 <- reactive({
    
    tr_data() |> 
      dplyr::select_if(where(is.numeric)) |> 
      dplyr::lag(2)
  })
  
  ts_lag3 <- reactive({
    
    tr_data() |> 
      dplyr::select_if(where(is.numeric)) |> 
      dplyr::lag(3)
  })
  
  ts_lag4 <- reactive({
    
    tr_data() |> 
      dplyr::select_if(where(is.numeric)) |> 
      dplyr::lag(4)
  })
  
  
  output$down_imp <- downloadHandler(
    
    filename = function() {
      paste("imputed_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(imp_data(), file, row.names = FALSE)
    }
  )
  

  output$down_raw <- downloadHandler(
    
    filename = function() {
      paste("raw_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(input_dataset(), file, row.names = FALSE)
    }
  )
  
  output$down_log <- downloadHandler(
    
    filename = function() {
      paste("log_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(log_data(), file, row.names = FALSE)
    }
  )
  
  output$down_f <- downloadHandler(
    
    filename = function() {
      paste("first_differenced_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(first_diff_data(), file, row.names = FALSE)
    }
  )
  
  output$down_lag1 <- downloadHandler(
    
    filename = function() {
      paste("first_lagged_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(ts_lag1(), file, row.names = FALSE)
    }
  )
  
  output$down_lag2 <- downloadHandler(
    
    filename = function() {
      paste("second_lagged_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(ts_lag2(), file, row.names = FALSE)
    }
  )
  
  output$down_lag3 <- downloadHandler(
    
    filename = function() {
      paste("third_lagged_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(ts_lag3(), file, row.names = FALSE)
    }
  )
  
  output$down_lag4 <- downloadHandler(
    
    filename = function() {
      paste("fourth_lagged_data", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(ts_lag4(), file, row.names = FALSE)
    }
  )
  
  #Time series models
  
  ts_df <- reactive({
    
    input_dataset() |> 
      dplyr::mutate(date_u =  input_dataset()[[1]]) |> 
      dplyr::select(
        dependent_var = input$y_var,
        date_u) |> 
      drop_na()
  })
  
  ts_df2 <- reactive({
    
    input_dataset() %>%
      dplyr::mutate(date_u =  input_dataset()[[1]]) |> 
      dplyr::select(
        dependent_var = input$y_var,
        date_u) |> 
      drop_na() |> 
      dplyr::lag()
  })
  
  #create time series objects
  
  mts <- reactive(
    
    ts(ts_df()$dependent_var, 
       start = lubridate::year(ts_df()$date_u)[1],
       frequency = 12)
  ) 
  
  #fit ARIMA models
  
  ts_fit <- reactive({
    
    if(is.null(input$file) & input$run6 < 0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input$file) & input$run6 > 0){
      req(input_dataset)
      
      #ts_fit <- Arima(mts(), order = c(1, 0, 0), seasonal = c(1, 0, 0))
      ts_fit <- auto.arima(mts())
    }
  })
  
  ts_fit2 <- reactive({
    
    if(is.null(input$file) & input$run7 < 0){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input$file) & input$run7 > 0){
      req(input_dataset)
      
      mts2 = ts(ts_df2()$dependent_var, 
                start = lubridate::year(ts_df2()$date_u)[2],
                frequency = 12)
      
      #ts_fit2 <- Arima(mts2, order = c(1, 0, 0), seasonal = c(1, 0, 0))
      ts_fit2 <- auto.arima(mts2)
    }
  })
  
  #KOYCK model
 
  #fit the model
  
  data_koy <- reactive({
    
    input_dataset() %>%
    dplyr::select(x = input$x_vars2, y = input$y_var) %>%
    drop_na()
    
    })
  
  koyck_model <- reactive({
    req(input_dataset())
    
    if(is.null(input$file) & is.null(input$x_vars2) & input$run8 < 0) {
      return("")
    }else if(!is.null(input$file) & !is.null(input$x_vars2) & input$run8 > 0){
      
      koyck_model = koyckDlm(x = data_koy()$x, 
                             y = data_koy()$y,
                             intercept = TRUE)
    }
  })
  
  ###VAR model
  
  var_df <- reactive(
    
    var_df <- input_dataset() |> 
      dplyr::select(input$x_vars)
  )
  
  ## fit VAR model
  
  var_fit <- reactive(
    
    var_fit <- VAR(var_df(), p=1, type="both")
  )
  
  # VECM model implementation
  
  data_vecm <- reactive(
    
    input_dataset() |> 
      dplyr::select(1, input$x_vars) |> 
      as.data.frame()
  )
  
  vecm_fit <- reactive({
    
    end_year   = year(data_vecm()[nrow(data_vecm()),1])
    start_year = year(data_vecm()[1,1])
    
    yq <- expand.grid(1:ncol(input_dataset()), 
                      start_year:end_year)[1:nrow(input_dataset()),]
    colnames(yq) <- c("q", "yyyy"); rownames(yq) <- NULL
    
    # quarterly centered dummy variables
    yq$Q1 <- (yq$q==1)-1/4
    yq$Q2 <- (yq$q==2)-1/4
    yq$Q3 <- (yq$q==3)-1/4
    dum_season <- yq[,-c(1,2)]
    
    VECM_tsDyn <-  VECM(data_vecm()[,-1], lag=1, r=2,
                        estim = "ML",
                        LRinclude = "none",
                        exogen    = dum_season)
  })
  
  # VARMA model implementation: Not yet implemented!
  
  
  
  
  # Cubic spline interpolation
  
  data_cubic <- reactive({
    
   
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
    req(input_dataset2)
      
    
    input_dataset2() |> 
      dplyr::select(quarter = 1,
                    input$series) |> 
      #ensure Q is replaced with q in the incoming data set
             mutate(quarter = as.Date(str_to_lower(quarter)),
                    quarter = as.yearqtr(quarter, format="%Yq%q"),
                    qvar    = as.Date(quarter))
    
 
    }
    
  })
  
  
  daily <- reactive(
    
    seq(data_cubic()$qvar[1], tail(data_cubic()$qvar, 1), 
        by = 'day')
    
  ) 
  
  quarterly_data <- reactive(
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
      data_cubic() |> 
      dplyr::select(
        qvar,
        initial_series = input$series
      )
      
    }
  )
  
  quarterly2 <- reactive(
    
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
    data.frame(qvar = daily(),
               interpolated_values = spline(quarterly_data(),
                                   method = 'natural',
                                   xout = daily())$y)
      
    }
  )
  
  
  interpolated_df <- reactive(
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
    
    merge(quarterly_data(), quarterly2(), by = 'qvar', all = TRUE)
      
    }
  )
  
  output$cubic_dt <- renderDT(
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
      
      interpolated_df()
      
    }
  )
  output$cubic_plot <- renderPlot(
    
    if(is.null(input$file2) & is.null(input$series) & input$run11 < 0) {
      return("")
    }else if(!is.null(input$file2) & !is.null(input$series) & input$run11 > 0){
      
    ggplot() +
      # geom_line(data = quarterly_data(), aes(x = qvar, y = input$series),
      #           color = "blue") +
      geom_path(data = quarterly2(),    aes(x = qvar, y = interpolated_values),
                color = "red",
                alpha = 0.8,
                linewidth = 0.8,
                linetype = 4) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "A plot for the Interpolated time series",
             subtitle = "The interpolation was done using the cubic spline method",
             x = "Year",
             y = "Quartely values") +
        theme_economist()
      
      
    }
    
  )
  
  # allow the user to download the interpolated data set - Eric 4th April 2025
  
    
    output$qtr <- downloadHandler(
      
      filename = function() {
        paste("interpolated_data", '.csv', sep = ",")
      },
      
      content = function(file) {
        
        write.csv(interpolated_df(), file, row.names = FALSE)
      }
    )
    

  
  #Render the transformed data sets
  
  output$first_data <- renderDT(
    
    if(is.null(input$file) & input$f_transform < 0){
      return("")
    }else if(!is.null(input$file) & input$f_transform > 0){
      data_koy()
    }
  )
  
  output$lag1_data <- renderDT(
    
    if(is.null(input$file) & input$lag1_transform < 0){
      return("")
    }else if(!is.null(input$file) & input$lag1_transform > 0){
      ts_lag1()
      
    }
  )
  
  output$lag2_data <- renderDT(
    
    if(is.null(input$file) & input$lag2_transform < 0){
      return("")
    }else if(!is.null(input$file) & input$lag2_transform > 0){
      ts_lag2()
    }
  )
  
  output$lag3_data <- renderDT(
    
    if(is.null(input$file) & input$lag3_transform < 0){
      return("")
    }else if(!is.null(input$file) & input$lag3_transform > 0){
      ts_lag3()
    }
  )
  
  output$lag4_data <- renderDT(
    
    if(is.null(input$file) & input$lag4_transform < 0){
      return("")
    }else if(!is.null(input$file) & input$lag4_transform > 0){
      ts_lag4()
    }
  )
  
  # Downloadable csv files for the coefficients and predictions
  
  coefficients1 <- reactive(
    
    if(!is.null(input$file) & input$run1 > 0) {
      fit1() |> 
        broom::tidy() |> 
        as.data.frame()
    }
    
  )
  
  coefficients2 <- reactive(
    
    if(!is.null(input$file) & input$run2 > 0){
    fit2() |> 
      broom::tidy() |> 
      as.data.frame()
      
    }
  )
  
  coefficients3 <- reactive(
    
    if(!is.null(input$file) & input$run3 > 0){
    fit_ridge() |> 
      coef() |> 
      as.matrix() |> 
      as.data.frame() 
    }
  )
  
  coefficients3_zero <- reactive(
    
    if(!is.null(input$file) & input$run3 > 0){
      fit_ridge_zero() |> 
        coef() |> 
        as.matrix() |> 
        as.data.frame() 
    }
  )
  
  
  coefficients4 <- reactive(
    
    if(!is.null(input$file) & input$run4 > 0){
    forward_fit() |> 
      broom::tidy() |> 
      as.data.frame()
    }
  )
  
  coefficients5 <- reactive(
    
    if(!is.null(input$file) & input$run5 > 0){
    backward_fit() |> 
      broom::tidy() |> 
      as.data.frame()
      
    }
  )
  
  coefficients6 <- reactive(
    
    if(!is.null(input$file) & input$id_orc > 0){
    fit1_corrected() |> 
      broom::tidy() |> 
      as.data.frame()
      
    }
  )
  
  coefficients7 <- reactive(
    
    if(!is.null(input$file) & input$run_z > 0){
    fit_zero() |> 
      broom::tidy() |> 
      as.data.frame()
      
    }
  )
  
  
  #### predicted values vs actual values
  
    
  preds1 <- reactive({
    
    if(!is.null(input$file)  & input$run1 > 0){
      
    pred1 = stats::predict(fit1())
    
    preds1 = input_dataset() %>%
      dplyr::select(1, input$y_var, input$x_vars) |> 
      drop_na() |> 
      dplyr::mutate(preds = pred1) |> 
      as.data.frame()
    }
  })
  

  
  preds2 <- reactive({
    
    if(!is.null(input$file) & input$run2 > 0 & input$run1 > 0){
      pred = stats::predict(fit2())
      
      std_error = summary(fit1())$sigma
      
      preds2 = input_dataset() %>%
        dplyr::select(1, input$y_var, input$x_vars) |> 
        drop_na() |> 
        dplyr::mutate(preds = pred,
                      residuals = summary(fit1())$residuals,
                      dummy1    = if_else(residuals >= std_error, 1, 0),
                      dummy2    = if_else(residuals < -std_error, 1, 0)) |> 
        dplyr::select(-residuals) |> 
        as.data.frame()
    }
    
  })
  
  preds_forward <- reactive({
    
    if(!is.null(input$file) & input$run4 > 0){
      pred = stats::predict(forward_fit())
      
      
      preds_forward = input_dataset() %>%
        dplyr::select(1, input$y_var, input$x_vars) |> 
        drop_na() |> 
        dplyr::mutate(preds = pred) |> 
        as.data.frame()
    }
    
  })
  
  preds_back <- reactive({
    
    if(!is.null(input$file) & input$run5 > 0){
      pred = stats::predict(backward_fit())
      
      
      preds_back = input_dataset() %>%
        dplyr::select(1, input$y_var, input$x_vars) |> 
        drop_na() |> 
        dplyr::mutate(preds = pred) |> 
        as.data.frame()
    }
    
  })
  
  #predictions for ridge regression
  
  preds_ridge <- reactive({
    
    if(!is.null(input) & input$run3 > 0){
      
      y = data_ridge()$dependent_var
      
      x = data_ridge() |> 
        dplyr::select(input$x_vars) |> 
        data.matrix()
      
      x_test = data_ridge() |> 
        dplyr::select(input$x_vars) |> 
        data.matrix()
      
      
      cv_model <- cv.glmnet(x, y, alpha = 0)
      
      best_lambda <- cv_model$lambda.min
      
      
      fit_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
      
      pred = glmnet::predict.glmnet(fit_ridge(), s = best_lambda, newx = x_test)
      
      
      preds_ridge = data_ridge() %>%
        dplyr::mutate(preds = pred) |> 
        as.data.frame()
    }
    
  })
  
  #predictions for time series regression
  
  preds_time <- reactive({
    
    if(!is.null(input$file) & input$run6 > 0){
      pred = forecast::forecast(ts_fit())
      y = as.numeric(pred$fitted)
      
      dat2_ <- input_dataset() |> 
        dplyr::select(
          input$y_var) |> 
        drop_na() 
      
      
      preds_time = dat2_ %>%
        dplyr::mutate(preds = y) |> 
        as.data.frame()
    }
  })
  
  preds_time2 <- reactive({
    
    if(!is.null(input$file) & input$run7 > 0){
      pred = forecast::forecast(ts_fit2())
      y = as.numeric(pred$fitted)
      
      dat2_ <- input_dataset() |> 
        dplyr::select(
          input$y_var) |> 
        drop_na() 
      
      
      preds_time2 = dat2_ %>%
        dplyr::mutate(preds = y) |> 
        as.data.frame()
    }
    
  })
  
  
  ###Download Handlers
  
  output$orc_d <- downloadHandler(
    
    filename = function() {
      paste("orcut_coefficients", '.csv', sep = ",")
    },
    
    content = function(file) {
      
      write.csv(coefficients6(), file, row.names = FALSE)
    }
  )
  
   output$dw3_ <- downloadHandler(

    filename = function() {
      paste("coefficients", ".xlsx", sep = "")
    },

    content = function(file) {
      
    if(!is.null(coefficients1())){
      write.xlsx2(coefficients1(), file, sheetName = 'model1_coefficients',
                  row.names = FALSE)
    }
     
    if(!is.null(coefficients2())){
      write.xlsx2(coefficients2(), file, sheetName = 'model2_coefficients',
                  row.names = FALSE, append = TRUE)
    }
     
     if(!is.null(coefficients3())){
       write.xlsx2(coefficients3(), file, sheetName = 'ridge_coefficients',
                   row.names = TRUE, append = TRUE)
     }
      
      if(!is.null(coefficients3_zero())){
        write.xlsx2(coefficients3_zero(), file, sheetName = 'ridge_coefficients_with_zero_intercept',
                    row.names = TRUE, append = TRUE)
      }
     
     if(!is.null(coefficients4())){
       write.xlsx2(coefficients4(), file, sheetName = 'coef_forward_elimination',
                   row.names = FALSE, append = TRUE)
     }
 
     
    if(!is.null(coefficients5())){
      write.xlsx2(coefficients5(), file, sheetName = 'coef_backward_elimination',
                  row.names = FALSE, append = TRUE)
    }
     

    if(!is.null(coefs_iv())){
      write.xlsx2(coefs_iv(), file, sheetName = 'coef_IV_regression',
                  row.names = FALSE, append = TRUE)
    }
     
     
    if(!is.null(coefficients7())){
      write.xlsx2(coefficients7(), file, sheetName = 'zero_intercept_coefficients',
                  row.names = FALSE, append = TRUE)
    }
     
      
    }
    
  )

  
  
  output$dw4_ <- downloadHandler(
    
    filename = function() {
      paste("predictions", ".xlsx", sep = "")
    },
    
    content = function(file) {
      
      if(!is.null(preds1())){
        write.xlsx2(preds1(), file, sheetName = 'model1_predictions',
                    row.names = FALSE)
      }
      
      if(!is.null(preds2())){
        write.xlsx2(preds2(), file, sheetName = 'model2_predictions',
                    row.names = FALSE, append = TRUE)
      }
      
      if(!is.null(preds_ridge())){
        write.xlsx2(preds_ridge(), file, sheetName = 'ridge_predictions',
                    row.names = FALSE, append = TRUE)
      }

      if(!is.null(preds_forward())){
        write.xlsx2(preds_forward(), file, sheetName = 'forward_model_predictions',
                    row.names = FALSE, append = TRUE)
      }

      if(!is.null(preds_back())){
        write.xlsx2(preds_back(), file, sheetName = 'backward_model_predictions',
                    row.names = FALSE, append = TRUE)
      }

      if(!is.null(preds_time())){
        write.xlsx2(preds_time(), file, sheetName = 'timeseries_model1_predictions',
                    row.names = FALSE, append = TRUE)
      }

      if(!is.null(preds_time2())){
        write.xlsx2(preds_time2(), file, sheetName = 'timeseries_model2_predictions',
                    row.names = FALSE, append = TRUE)
      }
      
      write.xlsx2(preds0_df(), file, sheetName = 'logistic_classification_preds',
                  row.names = FALSE, append = TRUE)
      
      write.xlsx2(preds1_df(), file, sheetName = 'decision_tree_predictions',
                  row.names = FALSE, append = TRUE)
      
      write.xlsx2(preds2_df(), file, sheetName = 'random_forest_predictions',
                  row.names = FALSE, append = TRUE)
      
      write.xlsx2(preds3_df(), file, sheetName = 'SVM_predictions',
                  row.names = FALSE, append = TRUE)
      
    }
  )
  
  
  ####pred vs actual data sets for use in plotting the graphs
  
  dat1_ <- reactive({
    
    pred1 = stats::predict(fit1())
    
    
    dat1 = dat() %>%
      dplyr::mutate(preds = pred1,
                    x = seq(1:nrow(dat())))
  })
  
  dat2_ <- reactive({
    
    pred = stats::predict(fit2())
    std_error = summary(fit1())$sigma
    
  
    dat3 = dat() %>%
      dplyr::mutate(residuals = summary(fit1())$residuals,
                    dummy1    = if_else(residuals >= std_error, 1, 0),
                    dummy2    = if_else(residuals < -std_error, 1, 0)) %>%
      dplyr::select(-residuals) %>%
      dplyr::mutate(preds = pred,
                    x = seq(1:nrow(dat())))
  })
  
  #model summary1
  
  output$model_summary1 <- renderPrint({
    req(input_dataset())
    
    if(input$run1 < 0){
      return(cat("Please run the model first!"))
    }else if(input$run1 > 0){
      cat("Multiple linear regression without dummy variables \n")
      fit1() |> 
        broom::tidy() |> 
        pander::pander()
      
      cat("Performance metrics \n")
      cat("R square:",round(summary(fit1())$r.squared, 3), "\n")
      cat("Adjusted R square:",round(summary(fit1())$adj.r.squared, 3))
    }
  })
  
  #test for heteroscedaciticity
  
  output$hetero <- renderPrint({
    req(input_dataset())
    
    if(input$run1 < 0){
      return(cat("Please run the model first!"))
    }else{
      if (is.null(input$x_vars) | is.null(input$file)) {
        return(cat("Select at least one predictor to test for heteroscedaciticity"))
      } 
      else if(!is.null(input$x_vars) & input$run1 > 0)
      {
        cat("Test for heteroscedasticity  \n")
        bptest(fit1())
      }
    }
  })
  
  #test for multicollinearity
  
  output$multi <- renderPrint({
    
    if(input$run1 < 0 ){
      return("Please run the model first!")
    }else {
      if (length(input$x_vars) < 2 & is.null(input$file)) {
        return(cat("Please select your data and run the model first!"))
      } 
      else if(!is.null(input$x_vars) & input$run1 > 0)
      {
        cat("Test for multicollinearity: Any VIF more than 10 indicate issues with multicollinearity. \n")
        
        vif = car::vif(fit1())
        
        df = tibble(variable = names(vif),
                    vif = vif)
        df |> 
          pander::pander()
      }
    }
  })
  
  # Durban Watson statistic
  
  output$auto <- renderPrint({
    
    #perform Durbin-Watson test
    if(is.null(input$file) & input$run1 < 0 ){
      return(cat("Please select your data and run the model first!"))
    }else if(!is.null(input$file) & input$run1 > 0 ){
      cat("Test for autocorrelation with Durban Watson statistic \n")
      durbinWatsonTest(fit1())
    }
  })
  
  
  ## Orcut - cochrane correction
  
  
  
  
  # Package ‘orcutt’ was removed from the CRAN repository.
  # 
  # Formerly available versions can be obtained from the archive.
  # 
  # Archived on 2024-11-04 as issues were not corrected despite reminders.
  # 
  # A summary of the most recent check results can be obtained from the check results archive.
  # 
  # Please use the canonical form https://CRAN.R-project.org/package=orcutt to link to this page.
  # 
  
  # The package orcutt was archived and no longer maintained. 
  # So maybe in the future we might need to look for other ways of
  # performing orcut-cochrane correction or just continue using the archived version.
  
  
  output$correct1 <- renderPrint(
    
    if(input$run1 < 0 ){
      return("")
    }else if(input$run1 > 0 ){
      fit1() |> 
        cochrane.orcutt(convergence = 5, max.iter = 1000)
    }
  )
  
  output$correct_orcutt1 <- renderPrint(
    
    if(input$id_orc < 0 ){
      return("")
    }else if(input$id_orc > 0 ){
      cat('Only consider the results below if there is a problem with autocorrelation, otherwise use the results from the first model.\n')
      cat("Linear regression results after Cochrane-orcutt correction for autocorrelation.\n")
      fit1_corrected() |> 
        broom::tidy() |> 
        pander::pander()
        
    }
  )
  
  
  output$correct_diff <- renderPrint(
    
    if(input$id_orc < 0 ){
      return("")
    }else if(input$id_orc > 0 ){
      
      cat("Linear regression results after first difference (lag1) correction for autocorrelation.\n")
      fit1_corrected_d() |> 
        broom::tidy() |> 
        pander::pander()
      
    }
  )
  
  
  
  
  #mode2 summary2
  
  output$model_summary2 <- renderPrint({
    req(input_dataset())
    
    if(is.null(input$file) & input$run2 < 0){
      return(cat("Please run the first model first to generate standard errors!"))
    }else if(!is.null(input$file) & input$run2 > 0){
      cat("Multiple linear regression with dummy variables \n")
      fit2() |> 
        broom::tidy() |> 
        pander::pander()
      
      cat("Performance metrics \n")
      cat("R square:",round(summary(fit2())$r.squared, 3), "\n")
      cat("Adjusted R square:",round(summary(fit2())$adj.r.squared, 3))
    }
  })
  
  #correction of autocorrelation due to cochrane orcutt
  
  output$correct_orcutt2 <- renderPrint(
    
    if(input$run2 < 0 ){
      return("")
    }else if(input$run2 > 0 ){
      
      cat("Linear regression results after Cochrane-orcutt correction\n")
      fit2_corrected() |> 
        broom::tidy() |> 
        pander::pander()
      
    }
  )
  
  #test for heteroscedaciticity
  
  output$hetero2 <- renderPrint({
    
    if(is.null(input$file) & input$run2 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run2 > 0 & input$run1 > 0){
      cat("Test for heteroscedasticity  \n")
      
      bptest(fit2())
    }
  })
  
  #test for multicollinearity
  
  output$multi2 <- renderPrint({
    
    if(is.null(input$file) & input$run2 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run2 > 0 & input$run1 > 0){
      cat("Test for multicollinearity: Any VIF more than 10 indicate issues with multicollinearity \n")
      
      vif = car::vif(fit2())
      
      df = tibble(variable = names(vif),
                  vif = vif)
      
      df |> 
        pander::pander()
    }
  })
  
  #DW statistic for model 2
  
  output$auto2 <- renderPrint({
    
    if(is.null(input$file) & input$run2 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run2 > 0 & input$run1 > 0){
      #perform Durbin-Watson test
      
      cat("Test for autocorrelation with Durban Watson statistic \n")
      durbinWatsonTest(fit2())
    }
   
  })
  
  output$correct2 <- renderPrint(
    
    if(input$run2 < 0){
      return("")
    }else if(input$run2 > 0){
      fit2() |> 
        cochrane.orcutt(convergence = 5, max.iter = 1000)
    }
  )
  
  #summary of ridge regression
  
  output$model_summary3 <- renderPrint({
    if(is.null(input$file) & input$run3 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run3 > 0){
      cat("Ridge regression Results \n")
      coef(fit_ridge()) 
    }
  } )
  
  output$model_summary3_zero <- renderPrint({
    if(is.null(input$file) & input$run3 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run3 > 0){
      cat("Ridge regression Results without the intercept \n")
      coef(fit_ridge_zero()) 
    }
  } )
  
  #metrics for ridge regression
  
  output$metrics3 <- renderPrint({
    
    if(is.null(input$file) & input$run3 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run3 > 0){
      y = data_ridge()$dependent_var
      
      x = data_ridge() |> 
        dplyr::select(input$x_vars) |> 
        data.matrix()
      
      cv_model <- cv.glmnet(x, y, alpha = 0)
      
      best_lambda <- cv_model$lambda.min
      
      y_predicted <- predict(fit_ridge(), s = best_lambda, newx = x)
      
      #find SST and SSE
      sst <- sum((y - mean(y))^2)
      sse <- sum((y_predicted - y)^2)
      
      #find R-Squared
      rsq <- 1 - sse/sst
      
      cat("Metrics for Ridge Regression \n")
      
      paste0("The value of R2 is",":", round(rsq, 3))
    }
   
  })
  
  #Summary for time series models
  
  output$model_summary4 <- renderPrint(
    if(is.null(input$file) & input$run6 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run6 > 0){
      summary(ts_fit())
    }
  )
  
  output$model_summary5 <- renderPrint(
    if(is.null(input$file) & input$run7 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run7 > 0){
      summary(ts_fit2())
    }
  )
  
  output$model_summary6 <- renderPrint(
    if(is.null(input$file) & input$run8 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & !is.null(input$x_vars2) & input$run8 > 0){
      summary(koyck_model(), diagnostics = TRUE)
    }
  )
  
  output$model_summary7 <- renderPrint(
    if(is.null(input$file) & input$run9 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file)  & input$run9 > 0){
      summary(var_fit())
    }
  )
  
  output$model_summary8 <- renderPrint(
    if(is.null(input$file) & input$run10 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file)  & input$run10 > 0){
      summary(vecm_fit()) 
    }
  )
  
  output$forward_results <- renderPrint(
    if(is.null(input$file) & input$run4 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run4 > 0){
      cat("Stepwise regression - Forward Results \n")
      
      forward_fit() |> 
        broom::tidy() |> 
        pander::pander()
      
      cat("Performance metrics \n")
      cat("R square:",round(summary(forward_fit())$r.squared, 3), "\n")
      cat("Adjusted R square:",round(summary(forward_fit())$adj.r.squared, 3))
      
    }
  )
  
  output$back_results <- renderPrint(
    if(is.null(input$file) & input$run5 < 0){
      return(cat("Please run the model first!"))
    }else if(!is.null(input$file) & input$run5 > 0){
      cat("Stepwise regression - Backward Results \n")
      backward_fit() |> 
        broom::tidy() |> 
        pander::pander()
      
      cat("Performance metrics \n")
      cat("R square:",round(summary(backward_fit())$r.squared, 3), "\n")
      cat("Adjusted R square:",round(summary(backward_fit())$adj.r.squared, 3))
    }
  )
  
  output$zero_results <- renderPrint({
    req(input_dataset())
    
    if(input$run_z < 0){
      return(cat("Please run the model first!"))
    }else if(input$run_z > 0){
      cat("Multiple linear regression results for zero intercept model \n")
      fit_zero() |> 
        broom::tidy() |> 
        pander::pander()
      
      cat("Performance metrics \n")
      cat("R square:",round(summary(fit_zero())$r.squared, 3), "\n")
      cat("Adjusted R square:",round(summary(fit_zero())$adj.r.squared, 3))
    }
  })
  
  #graphs
  
  output$plot <- renderPlot({
    req(input_dataset)
    
    if(input$run1 > 0){
      plot(dat1_()$x, y = dat1_()$dependent_var, type = 'l', pch = 19, col = "blue",
           ylab = "", xlab = "", lwd = 4,
           main = paste(input$y_var, "versus its predicted values for model 1",
                        collapse = ""))
      lines(dat1_()$x, y = dat1_()$preds, type = 'l', pch = 19, col = "orange",
            ylab = "", xlab = "", lwd = 4)
      legend("bottomright", legend = c(input$y_var, "Predicted values"),
             col=c("blue", "orange"), lty = c(1,1), cex=0.8)
    }
    
  })
  
  #plot for model 2
  
  output$plot1 <- renderPlot({
    req(input_dataset)
    
    if(input$run2 > 0){
      plot(dat2_()$x, y = dat2_()$dependent_var, type = 'l', pch = 19, col = "blue",
           ylab = "", xlab = "", lwd = 4,
           main = paste(input$y_var, "versus its predicted values for model 2",
                        collapse = ""))
      lines(dat2_()$x, y = dat2_()$preds, type = 'l', pch = 19, col = "orange",
            ylab = "", xlab = "", lwd = 4)
      legend("bottomright", legend = c(input$y_var, "Predicted values"),
             col=c("blue", "orange"), lty = c(1,1), cex=0.8)
    }
 
  })
  
  #Time series plot
  
  ts_plot <- reactive(
    
    #use ggplot for an elegant graph
    
      mts() %>%
        ggtsdisplay(main = paste0("Time series plot for \n", input$y_var, collapse = ""),
                    plot.type = 'partial',
                    ylab = input$y_var,
              theme = theme_economist()) 
      
      # Its working now after updating the R version: 8th of Feb 2025.
      
    # Had to remove theme because of errors with ggplot2 package, hope this will be fixed in the future
     

  )
  
  
  output$plot2 <- renderPlot({
    
    ts_plot()
  })

  
  
  
  ######### SAVING VARIOUS DATA SETS TO THE DATA BASE
  ### THIS IS STILL NOT WORKING --- FURTHER WORK IS NEEDED HERE ######
  
  
  ### Working now as of 17th of Dec 2024 - just need to figure out how to handle
  ### user authentication - passwords and usernames - should we expose these
  ### in the online shiny app?--should we request these credentials from each user? ---.
  
source('secrets.R')

con <- reactive({

  con <- dbConnect(RMySQL::MySQL(),
                   dbname = 'google_trends',
                   host = options()$mysql$host,
                   port = options()$mysql$port,
                   user = options()$mysql$user,
                   password = options()$mysql$password)
})
  

    observeEvent(
      
      input$db, {
        tryCatch(
          {
            dbBegin(con())
            
            if(!is.null(preds1()) & input$db > 0){
              
              dbWriteTable(con(), name = "pred_test1", value = preds1(),
                           overwrite = TRUE)
            }
            
           
            if ('pred_test1' %in% dbListTables(con())) {
              showNotification(
                markdown(
                  glue::glue(
                    "Table Successfully inserted into the database."
                  )
                ),
                type = "message"
              )
            }
            
            dbCommit(con())
          },
          error = function(e) {
            dbRollback(con())
            showNotification("Error: Failed to update database.", type = "error")
          }
        )
      }
      
    )
    
  
    # Disconnect from DuckDB when the app stops
    # onSessionEnded(function() {
    #   dbDisconnect(con())
    # })
    # 
  
  # save <- reactive(
  # 
  #   if(!is.null(preds1()) & input$db > 0){
  # 
  #     # Save the data to MySQL
  #     dbWriteTable(con(), name = 'pred_test', value = preds1())
  # 
  #   }
  # 
  # )

  
  # output$db <- downloadHandler(
  #   
  #   filename = function() {
  #     paste("preds1_db", sep = "")
  #   },
  #   
  #   content = function(file) {
  #     
  #     if(!is.null(preds1()) & input$db > 0){
  #       dbWriteTable(con(), name = 'pred_test', value = preds1())
  #     }
  #   }
  # )
  

}
