library(testthat)
library(dplyr)
library(reshape2)

# Setting up workspace, install 'rstudioapi' if using for first time
#install.packages('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

source('../homework_check_functions.R')

# Setup variables ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Homework & test file locations (the '.' will not show it in environment view, 
#   ensuring that they will not be overwritten)
.tests_file = "HW3_testid.R"
.submission = "test_submission.R"

# Structure of tests and how many unit tests there are per assignment
.tests_structure =
  data.frame(
    context = as.character(Sys.time()),
    submission = 'Total nr of unit tests',
    "Ülesanne 1.1.1" = 9.0, 
    "Ülesanne 1.2.1" = 7.0, 
    "Ülesanne 2.1.1" = 6.0,
    "Ülesanne 2.2.1" = 4.0,
    "Ülesanne 2.3.1" = 4.0,
    "Ülesanne 3.1.1" = 8.0,
    "Ülesanne 3.2.1" = 5.0,
    "Ülesanne 3.3.1" = 7.0,
    "Ülesanne 3.4.1" = 8.0,
    "Ülesanne 3.5.1" = 4.0,
    "Ülesanne 3.6.1" = 3.0
  ) %>%
  # Add total number of tests
  mutate(Total = rowSums(.[-c(1:2)]))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Test and write results into a Google Spreadsheet
homework_check(.tests_file, .tests_structure)
