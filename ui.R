

source("helper_functions.R")


ui <- fluidPage(theme = shinytheme("cerulean"), 
                title = "Model Builder App",
                use_shiny_title(),
                titlePanel("Model Builder: Load Data and Build some Models!"),
                sidebarPanel( width = 3,
                              
                              fileInput(inputId = "file",
                                        label = "Please select an Excel file"),
                              
                              numericInput(inputId = "sheet_index",
                                           label = "Please choose the sheet index to upload",
                                           min = 1,
                                           max = 10,
                                           value = 1),
                              
                              sliderInput(inputId = "prop",
                                          label = "Proportion for the training set",
                                          value = 0,
                                          min   = 0,
                                          max   = 1,
                                          step  = 0.05),
                              
                              selectInput(inputId = "y_var",
                                          label = "select continuous outcome variable",
                                          choices = "",
                                          multiple = FALSE),
                              
                              selectInput(inputId = "x_vars",
                                          label = "select predictor variables",
                                          choices = "",
                                          multiple = TRUE),
                              
                              selectInput(inputId = "tr_vars",
                                          label = "select variables to transform",
                                          choices = "",
                                          multiple = TRUE),
                              
                                pmap(vars[1,], mySelectInput),
                                pmap(vars[2,], mySelectInput),
                                pmap(vars[3,], mySelectInput),
                              
                                
                              
                              
                              
                ),
                
                mainPanel(
                  navbarPage("",
                             id = 'datasets',
                             tabPanel(
                               title = "App Information",
                               includeMarkdown("readme2.md")
                             ),
                             
                             navbarMenu("Transform Data",

                                        tabpanFun1(id = "raw_data1",      id2 = "down_raw",  label = "Raw data", OutputId = "raw_data"),
 
                                        tabpanFun1(id = "l_transform",    id2 = "down_log",  label = "Log transform", OutputId = "log_data"),

                                        tabpanFun1(id = "f_transform",    id2 = "down_f",    label = "First differences", OutputId = "first_data"),

                                        tabpanFun1(id = "lag1_transform", id2 = "down_lag1", label = "First lag", OutputId = "lag1_data"),

                                        tabpanFun1(id = "lag2_transform", id2 = "down_lag2", label = "Second lag", OutputId = "lag2_data"),

                                        tabpanFun1(id = "lag3_transform", id2 = "down_lag3", label = "Third lag", OutputId = "lag3_data"),

                                        tabpanFun1(id = "lag4_transform", id2 = "down_lag4", label = "Fourth lag", OutputId = "lag4_data")
                                        

                             ),
                             
                             navbarMenu("Model Results",

                                        tabpanFun2(title = "Model results - First model",id1 = "model_summary1", id3 = "hetero", id4 = "multi", id5 = "auto",
                                                   id6 = "correct1", id7 = "downloadPreds1", id8 = "downloadCoef1", run_id = "run1"),

                                        tabpanFun2(title = "Model results - Second model",id1 = "model_summary2", id3 = "hetero2", id4 = "multi2", id5 = "auto2",
                                                   id6 = "correct2",id7 = "downloadPreds2", id8 = "downloadCoef2", run_id = "run2"),

                                        tabpanFun2(title = "Model results - Ridge Regression",
                                                   id1 = "model_summary3", id2 = "metrics3", id7 = "downloadPreds3", run_id = "run3"),
                                        
                                        tabpanFun2(title = "Forward  elimination method", id1 = "forward_results",
                                                   id7 = "downloadPreds4", id8 = "downloadCoef4", run_id = "run4"),
                                        
                                        tabpanFun2(title = "Backward elimination method", id1 = "back_results",
                                                   id7 = "downloadPreds5", id8 = "downloadCoef5", run_id = "run5")
                             ),

                             navbarMenu("Time series models",

                                        tabpanFun2(title = "Time series model", id1 = "model_summary4", id7 = "downloadPreds6", run_id = "run6"),
                    
                                       
                                        tabpanFun2(title = "Time series model with lag 1", id1 = "model_summary5", id7 = "downloadPreds7", run_id = "run7"),
                                        tabPanel(
                                          title = "Time series model: KOYCK",
                                          
                                          verbatimTextOutput("model_summary6"),
                                    
                                          #downloadButton("downloadPreds8", "Predictions"),
                                          selectInput(inputId = "x_vars2",
                                                      label = "select a single predictor",
                                                      choices = "",
                                                      multiple = TRUE),
                                          actionButton("run8", "Click here to run the model"),
                                          
                                          
                                        ),
                                        tabpanFun2(title = "Time series model: VAR", id1 = "model_summary7", id7 = "downloadPreds9",  run_id = "run9"),
                                        tabpanFun2(title = "Time series model: VECM", id1 = "model_summary8",id7 = "downloadPreds10", run_id = "run10")
                             ),
                             
                             
                             navbarMenu("Visualization",
                                        tabPanel(
                                          title = "Visualize Model 1",
                                          plotOutput("plot")
                                        ),
                                        
                                        tabPanel(
                                          title = "Visualize Model 2",
                                          plotOutput("plot1")
                                        ),
                                        
                                        tabPanel(
                                          title = "Visualize Time series",
                                         
                                          plotOutput("plot2"),
                                          plotOutput("acf"),
                                          plotOutput("pacf"),
                                          verbatimTextOutput("dicky_test")
                                          
                                        )
                             ),
                             
                             navbarMenu("Machine learning models",
                                        
                                        tabPanel(
                                          
                                          title = "Decision tree model",
                                          actionButton("idn1", "Click here to run the model"),
                                          downloadButton("dw1","Download predictions"),
                                          verbatimTextOutput("metrics1"),
                                          DTOutput("preds1")
                                        ),
                                        
                                        tabPanel(
                                          title = "Random forest model",
                                          actionButton("idn2", "Click here to run the model"),
                                          downloadButton("dw2","Download predictions"),
                                          verbatimTextOutput("metrics2"),
                                          DTOutput("preds2")
                                        ),
                                        
                                        tabPanel(
                                          title = "SVM model",
                                          actionButton("idn33", "Click here to run the model"),
                                          downloadButton("dw33","Download predictions"),
                                          verbatimTextOutput("metrics33"),
                                          verbatimTextOutput("svm_summary"),
                                          DTOutput("preds3")
                                        )
                                        
                             ),
                             
                              tabPanel(
                                title = "IV Regression",
                                actionButton("idn3","Click here to run the model"),
                                #downloadButton("iv_summary1", "Summary report"),
                                downloadButton("dw3","Download coefficients"),
                                
                                verbatimTextOutput("iv_summary")
                              
                             ),
                             
                             # tabPanel(
                             #   title = "Download time series data",
                             #   downloadButton("scrape","Download data"),
                             #   DTOutput("scrape1")
                             # )
                             
                             #   column(4, pmap(vars[1,], mySelectInput)),
                             #   column(4, pmap(vars[2,], mySelectInput)),
                             #   column(4, pmap(vars[3,], mySelectInput)),
                             #   actionButton("idn3","Click here to run the model"),
                             #   downloadButton("iv_summary1", "Summary report"),
                             #   downloadButton("dw3","Download coefficients"),
                             #   
                             #   verbatimTextOutput("iv_summary")
                  )
                )
)
