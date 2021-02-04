# ***************************************************************************************************
# Create Date:        2020-10-26
# Author:             Kaur Lumiste
# Description:        Contains functions that check all provided files according to a provided tests 
#                     file. Results are written into a Google Spreadsheet.
# Used for:           Automatic testing of homeworks
#***************************************************************************************************/

comment <- function(string){
  # formats strings to create VPL comments
  cat(paste0('Comment :=>> ', string, '\n')) 
}

grade <- function(num){
  # formats strings to create VPL comments
  cat(paste0('Grade :=>> ', num, '\n')) 
}

homework_check <- function(.tests_file, .tests_structure){
  #' '''''
  #' @description Initiates unit tests using testthat package. Requires a file to test and 
  #' file that includes all the tests.
  #' @param '.submission' - path to the homework submission
  #' @param '.tests_file' - path to the file that includes all the tests
  #' '''''
  
  # Apply tests
  output <- test_file(.tests_file, reporter = SilentReporter) %>% 
    as.data.frame()
  
  # Calculate grade
  output %>%
    summarise(grade = round((1 - sum(failed) / sum(nb))*100)) %>% 
    grade()
    
  # Tests skipped
  if(any(output$skipped)){
    comment('Syntax errors!')

    output %>% filter(skipped) %>%
      rowwise() %>% 
      mutate(message = unlist(result, recursive = F)[[1]],
             message = paste0('VIGA - ', test, '\n', message)) %>% 
      select(message) %>% 
      pull() %>% 
      comment()
  }
  
  # Tests failed
  if(sum(output$failed) > 0){
    comment('Some mistakes were fount in your solution')
    
    output %>% 
      filter(failed > 0 & !skipped) %>%
      rowwise() %>% 
      mutate(message = unlist(result, recursive = F)[[1]],
             message = paste0('VIGA - ', test, '\n', message)) %>% 
      select(message) %>% 
      pull() %>% 
      comment()
  }
  
}


code_run_test <- function(code, ylesanne = NULL){
  #' '''''
  #' @description Tries to run code and if it fails, then return error and skip all tests .
  #' @param 'code' - character vector of code lines to run
  #' '''''
  
  # Read and evaluate code lines
  for(i in 1:lenght(code)){
    read.commands = tryCatch(eval(parse(text = paste(code[i], collapse = '\n'))), condition = identity)
  
    # If error while running the exercise code, then skip all following tests
    if(any(class(read.commands) == "error")) {
      fail(message = paste0("Ei suutnud käivitada ", ylesanne, " koodi!"),
           info = paste0(read.commands$message)
      )
      
      skip("")
    }
  }
  
  return(NULL)
}
