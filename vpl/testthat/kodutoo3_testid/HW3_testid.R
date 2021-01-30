library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 3 kontroll")

# Ülesanne 1.1.1 õige lahendus -----
if(FALSE){
  #1
  summary(iris)
  keskmised = by(iris$Petal.Length, iris$Species, mean)
  
  #2
  #setosa
  
  #3
  iris$sordinimi = factor(iris$Species, levels= c("versicolor","setosa","virginica"))
  
  #4
  maksimumid = tapply(iris$Petal.Length, iris$sordinimi,max)
  maksimumid
}

ylesanne = "Ülesanne 1.1.1"
yl = 1

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            #1
            keskmised_test = by(iris$Petal.Length, iris$Species, mean)
            expect_equal(object = keskmised,
                         expected = keskmised_test,
                         info = paste0(ylesanne, ".1: "))
            
            #3
            expect_true("sordinimi"%in%names(iris),
                        info = paste0(ylesanne, ".3: puudub tunnus nimega 'sordinimi'"),
                        label = paste0(ylesanne, ".3 tunnuse olemasolu kontroll"))
            
            levels_test = c("versicolor","setosa","virginica")
            expect_equal(object = levels(iris$sordinimi),
                         expected = levels_test,
                         info = paste0(ylesanne, ".3: faktori väärtuste järjekord ei vasta oodatule"))
            
            #4
            maksimumid_test = tapply(iris$Petal.Length, iris$sordinimi,max)
            expect_equal(object = maksimumid,
                         expected = maksimumid_test,
                         info = paste0(ylesanne, ".4: "))
          })

# Ülesanne 1.2.1 õige lahendus -----
if(FALSE){
  #1
  intervallid = cut(iris$Petal.Length,breaks = seq(1,7,0.5),right=FALSE)
  
  #2
  is.factor(intervallid)
  
  #3
  sagedustabel = table(intervallid)
  sagedustabel
  
  #4
  tyhjad = 2
}

ylesanne = "Ülesanne 1.2.1"
yl = 2

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))

            
            
          })




# Ülesanne 2.1.1 õige lahendus -----
if(FALSE){
  x <- c(2:1, 2:1, 2:1, 4)
  y <- c(7, 1, 5, 2, 6, 3, 4)
  xy <- data.frame(x, y)
  x; y; xy
  
  #1
  xy1 = xy[order(x,y),]
  xy1
  
  #2
  xy2 = xy[order(x,-y),]
  xy2
  
  #3
  ##
}

ylesanne = "Ülesanne 2.1.1"
yl = 3

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 2.2.1 õige lahendus -----
if(FALSE){
  #1
  iris.sort1 = iris[order(iris$Sepal.Width, iris$Sepal.Length,iris$Petal.Width),]
  
  #2
  iris.sort1$sordinimi[nrow(iris)-1]
}

ylesanne = "Ülesanne 2.2.1"
yl = 4

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 2.3.1 õige lahendus -----
if(FALSE){
  #1
  iris.sort2 = iris[order(iris$Sepal.Width,-iris$Sepal.Length),]
  
  #2
  iris.sort2$sordinimi[30]
}

ylesanne = "Ülesanne 2.3.1"
yl = 5

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })



# Ülesanne 3.1.1 õige lahendus -----
if(FALSE){
  B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
  B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]
  
  #1
  str(B)
  library(reshape2)
  
  #2
  testid.pikk = melt(B,id.vars = 1, measure.vars = 3:42)
  
  #3
  str(testid.pikk)
}

ylesanne = "Ülesanne 3.1.1"
yl = 6

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 3.2.1 õige lahendus -----
if(FALSE){
  rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
                    colClasses = c("numeric", "character", "character", "factor"))
  summary(rotid)
  #1
  library(reshape2)
  
  #2
  rotid.lai = dcast(rotid, Rat+Diet ~ Time, value.var = "weight")
  
  #3
  rotid.lai
}

ylesanne = "Ülesanne 3.2.1"
yl = 7

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 3.3.1 õige lahendus -----
if(FALSE){
  #1
  tabel1 = table(rotid$Rat)
  tabel1
  
  #2
  tabel2 = dcast(rotid, Rat~"sagedus")
  tabel2
  
  #3
  # 3
}

ylesanne = "Ülesanne 3.3.1"
yl = 8

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 3.4.1 õige lahendus -----
if(FALSE){
  #1
  tabel3 = dcast(rotid, Rat~"kaalu mediaan",value.var = "weight", fun.aggregate = median)
  tabel3
  #2
  rott2mediaan = 240.0
}

ylesanne = "Ülesanne 3.4.1"
yl = 9

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })

# Ülesanne 3.5.1 õige lahendus -----
if(FALSE){
  arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                           sep = "\t", dec = ",", header = T, check.names = FALSE)
  #1
  arstiabita
  
  #2
  pikk = melt(arstiabita,variable.name = "Aasta")
  pikk
  
  #3
  transponeeritud = dcast(pikk, Aasta~Arstiabiliik)
  transponeeritud
}

ylesanne = "Ülesanne 3.5.1"
yl = 10

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })


# Ülesanne 3.6.1 õige lahendus -----
if(FALSE){
  #1
  transponeeritud = recast(arstiabita, variable~Arstiabiliik)
  transponeeritud
}

ylesanne = "Ülesanne 3.6.1"
yl = 11

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            
            
          })

