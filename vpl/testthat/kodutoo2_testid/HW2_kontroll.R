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
.tests_file = "HW2_testid.R"
.submission = "test_submission.R"

# Structure of tests and how many unit tests there are per assignment
.tests_structure =
  data.frame(
    context = as.character(Sys.time()),
    submission = 'Total nr of unit tests',
    "Ülesanne 1.1.1" = 7,
    "Ülesanne 1.2.1" = 1,
    "Ülesanne 1.3.1" = 7,
    "Ülesanne 1.4.1" = 7,
    "Ülesanne 2.1.1" = 4,
    "Ülesanne 2.2.1" = 7,
    "Ülesanne 2.3.1" = 5,
    "Ülesanne 2.4.1" = 3,
    "Ülesanne 2.5.1" = 11
  ) %>%
  # Add total number of tests
  mutate(Total = rowSums(.[-c(1:2)]))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


homework_check( .tests_file, .tests_structure)

