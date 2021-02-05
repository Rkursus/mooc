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
.tests_file = "HW5_testid.R"
.submission = "test_submission.R"

# Structure of tests and how many unit tests there are per assignment
.tests_structure =
  data.frame(
    context = as.character(Sys.time()),
    submission = 'Total nr of unit tests',
    "Ülesanne 1.1" = 7.0,
    "Ülesanne 2.1" = 11.0,
    "Ülesanne 3.1" = 21.0,
    "Ülesanne 4.1" = 12.0,
    "Ülesanne 5.1" = 18.0,
    "Ülesanne 6.1" = 16.0,
    "Ülesanne 7.1" = 16.0,
    "Ülesanne 8.1" = 13.0,
    "Ülesanne 9.1" = 14.0,
    "Ülesanne 10.1" = 8.0
  ) %>%
  # Add total number of tests
  mutate(Total = rowSums(.[-c(1:2)]))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Test and write results into a Google Spreadsheet
homework_check(.tests_file, .tests_structure)

