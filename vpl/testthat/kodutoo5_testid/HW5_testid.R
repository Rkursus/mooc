library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 5 kontroll")

# Ülesanne 1.1 õige lahendus -----
if(FALSE){
  A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
  head(A)
  
  #1
  library(dplyr)
  
  #2
  A1 <- mutate(A, kmi=(kaal)/(kasv/100)**2,
               kaalugrupp=ifelse(kmi<=25, "ala voi normkaal","ylekaal"))
  
  #3
  str(A1)
}

ylesanne = "Ülesanne 1.1"
yl = 1

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            #1
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 library() funktsiooni kontroll"))
            # expect_true(length(grep('library\\("dplyr"\\).*?', tmp_part)) == "dplyr", 
            #             info = paste0(ylesanne, ".1: paketinimi kirjutatakse ilma jutumärkideta"),
            #             label = paste0(ylesanne, ".1 library() funktsiooni sisendi kontroll"))
            # expect_true(length(grep("library\\('dplyr'\\).*?", tmp_part)) == 'dplyr', 
            #             info = paste0(ylesanne, ".1: paketinimi kirjutatakse ilma jutumärkideta"),
            #             label = paste0(ylesanne, ".1 library() funktsiooni sisendi kontroll"))
            
            #2
            expect_true(ncol(A1)==10,
                        info = paste0(ylesanne,".2: tabelisse pole lisatud 2 uut veergu"),
                        label = paste0(ylesanne, ".2 veergude arvu kontroll"))
            
            expect_true(sum(colnames(A1)[9:10] == c("kmi","kaalugrupp"))==2,
                        info = paste0(ylesanne,".2: ei leia veerge 'kmi' ja 'kaalugrupp'"),
                        label = paste0(ylesanne, ".2 veergude nimede kontroll"))
            
            expect_true(round(sum(A1$kmi),2)==1195.78,
                        info = paste0(ylesanne,".2: veeru 'kmi' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'kmi' väärtuste kontroll"))
            expect_true(sum(A1$kaalugrupp=="ylekaal")==39,
                        info = paste0(ylesanne,".2: veeru 'kaalugrupp' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'kaalugrupp' väärtuste kontroll"))
            #3
            expect_true(length(grep("str\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: vaja kasutada funktsiooni 'str'"),
                        label = paste0(ylesanne, ".3 str() funktsiooni kontroll"))
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