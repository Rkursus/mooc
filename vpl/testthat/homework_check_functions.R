# ***************************************************************************************************
# Create Date:        2019-10-26
# Author:             Kaur Lumiste
# Description:        Contains functions that check all provided files according to a provided tests 
#                     file. Results are written into a Google Spreadsheet.
# Used for:           Automatic testing of homeworks
#***************************************************************************************************/

apply_tests <- function(filename, tests_file){
  #' '''''
  #' @description Function that initiates tests using testthat package. Requires a file to test and 
  #' file that includes all the tests.
  #' @param 'filename' - path to the homework submission
  #' @param 'tests_file' - path to the file that includes all the tests
  #' '''''

  # Create a new enviroment
  #tstEnv <- new.env()
  
  # Read file
  #read.file = try(sys.source(file = filename, envir = tstEnv, toplevel.env = tstEnv))
  read.file = try(source(filename, encoding = "UTF-8", verbose = F, echo = F, print.eval = F, max.deparse.length = 1))
  
  # If error in reading file, then exit function. Else run tests
  if(class(read.file)=="try-error") {
    
    message(paste0("FAILED: Reading file ", filename, " - FAILED!"))
    return(NULL)
    break
    
  } else {
    
    test_results <- test_file(tests_file, reporter = SilentReporter) %>% 
      as.data.frame() %>% 
      mutate(submission = filename) %>% 
      select(submission, everything())
    
    return(test_results)
  }
}


homework_check <- function(.submission, .tests_file, .tests_structure){

  # Clean environment from previous results
  rm(list=ls()[!ls() %in% c("filename", 'homework_check')])
    
  # Apply tests in a separate environment
  .output <- apply_tests(.submission, .tests_file)
    
  
  # Recast and format the results to show only relevant info
  final_output <- .output %>% 
    dcast(context + submission ~ test, value.var = "failed") %>% 
    mutate(Total = rowSums(.[-c(1:2)])) 
  
  colnames(.tests_structure) <- colnames(final_output)
  final_output[, 3:ncol(.tests_structure)] <-
    round((1 - mapply("/", final_output[, 3:ncol(final_output)], .tests_structure[3:ncol(.tests_structure)]))  *
            100, 1) 
  
  final_output <- rbind(.tests_structure, final_output)
  
  final_output <- final_output %>% 
    left_join(.output %>% 
                mutate(test = paste0(test,"_error")) %>% 
                dcast(context + submission ~ test, value.var = "error"))
  
  
}
