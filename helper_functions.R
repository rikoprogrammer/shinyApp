
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
library(zoo)
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
library(matlib)

oldOpt <- options()
options(xlsx.date.format="dd MMM, yyyy")


#  a helper function to generate multiple select input statements in the UI

vars <- tibble::tribble(
  
  ~id, ~ label, ~ multiple,
  "y_var", "select continuous outcome variable", FALSE,
  "series", "choose a series to interpolate", FALSE,
  "x_vars", "select predictor variables", TRUE,
  "tr_vars", "select variables to transform", TRUE,
  "endo_vars", "endogenous variables", TRUE,
  "exo_vars", "exogenous variables", TRUE,
  "iv_vars", "instrument variables", TRUE,
  "xa","choose some independent variables", TRUE
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


# Constrained regression convertor

convertor <- function(X, Y) {
  
  cat("Matrix X \n")
  print(X)
  cat("Matrix Y \n")
  print(Y)
  
  # I need to add a  column of 1 in the X for the constant 
  X <- cbind(1, X)
  
  # Number of Rows 
  n <- nrow(X)
  
  # Number of Columns 
  k <- ncol(X)
  
  r1_r <- k  
  
  r1_c <- 1 
  
  r2_r <- k  
  
  r2_c <- k+1 
  
  # Set the size of the identity matrix
  n <- k-1  # Change this for the desired size
  
  # Create the identity matrix (top block)
  identity_block <- diag(1, n)
  
  # Create a single row of -1s
  negative_row <- matrix(-1, nrow = 1, ncol = n)
  
  # Combine the blocks vertically
  R2 <- rbind(identity_block, negative_row)
  
  # Create  column vector (matrix)
  n <- k  # Define the size for the identity matrix
  
  # Create a column matrix where first n-1 elements are 0 and the last is 1
  R1 <- matrix(c(rep(0, n-1), 1), ncol = 1)
  
  pi_sub = inv( t(X%*%R2) %*% (X%*%R2))   %*% t(X%*%R2) %*% (Y - (X%*%R1))
  pi_res=  R1 + (R2%*%pi_sub)

  return(pi_res)
  
}


