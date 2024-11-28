
#load packages

library(shiny)
library(car)
library(lmtest)
library(tidyverse)
library(forecast)
library(tidyquant)
library(glmnet)
library(DT)
library(shinythemes)
library(bslib)
library(shinytitle)
library(purrr)
library(lubridate)
library(ggthemes)
library(MASS)
library(tseries)
library(orcutt)
library(readxl)
library(dLagM)
library(tidymodels)
library(ranger)
library(psych)
library(rpart)
library(vip)
library(DT)
library(AER)
library(markdown)
library(e1071)
library(vars)  
library(tsDyn)
library(thematic)
library(xlsx)
library(DBI)

oldOpt <- options()
options(xlsx.date.format="dd MMM, yyyy")


#  a helper function to generate multiple select input statements in the UI

vars <- tibble::tribble(
  
  ~id, ~ label, ~ multiple,
  "y_var", "select continuous outcome variable", FALSE,
  "x_vars", "select predictor variables", TRUE,
  "tr_vars", "select variables to transform", TRUE,
  "endo_vars", "endogenous variables", TRUE,
  "exo_vars", "exogenous variables", TRUE,
  "iv_vars", "instrument variables", TRUE,
)

mySelectInput <- function(id, label, multiple) {
  selectInput(id, label, multiple = multiple, choices = "")
}


#A helper function for generating tab sets for transformation tabs

tabpanFun1 <- function(id, id2 = NULL, label, label2 = "Download Me!", OutputId, ...) {
  
  tabPanel(
    title = label,
    actionButton(id, label),
    downloadButton(id2, label2),
    DTOutput(OutputId)
  )
}

#A helper function for generating tab sets for various models

tabpanFun2 <- function(title, id1 = NULL, 
                       id2 = NULL, id3 = NULL, id4 = NULL, id5 = NULL, id6 = NULL, 
                       id7 = NULL,id8=NULL, id9=NULL, run_id = NULL, ...) {
  
  tabPanel(
    title = title,
    actionButton(run_id, "Click here to run the model"),
    verbatimTextOutput(id1),
    verbatimTextOutput(id2),
    verbatimTextOutput(id3),
    verbatimTextOutput(id4),
    verbatimTextOutput(id5),
    verbatimTextOutput(id6),
    verbatimTextOutput(id7),
    verbatimTextOutput(id8)
    
  )
}



