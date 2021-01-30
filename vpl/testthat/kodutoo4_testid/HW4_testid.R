library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 4 kontroll")

# Ülesanne 1.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 1.1"
yl = 1

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 2.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 2.1"
yl = 2

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 3.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 3.1"
yl = 3

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 4.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 4.1"
yl = 4

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 5.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 5.1"
yl = 5

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 6.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 6.1"
yl = 6

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 7.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 7.1"
yl = 7

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 8.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 8.1"
yl = 8

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 9.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 9.1"
yl = 9

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 10.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 10.1"
yl = 10

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })