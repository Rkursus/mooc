###%Ülesanne 1.1.1 lahendus

iris
summary(iris)

#1
keskmised <- by(iris$Petal.Length, iris$Species, mean)
keskmised

#2
#"setosa"

#3
iris$sordinimi <- factor(iris$Species, levels = c("versicolor", "setosa", "virginica"))
iris

#4
maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, max)
maksimumid

###%Ülesanne 1.2.1 lahendus

#1
intervallid <- cut(iris$Petal.Length, breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7), right = F)

#2
is.factor(intervallid)

#3
sagedustabel <- table(intervallid)
sagedustabel

#4
tyhjad <- 2
  
###%Ülesanne 2.1.1 lahendus

x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)
  
x; y; xy
  
#1
xy1 <- xy[order(x, y), ]
xy1
  
#2
xy2 <- xy[order(x, -y), ]
xy2

#3
#xy1 puhul nii x kui y kasvavalt, xy2 puhul x on kasvav ja y on kahanev

###%Ülesanne 2.2.1 lahendus

head(iris)

#1
iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width), ]

#2
tail(iris.sort1)
#"setosa"

###%Ülesanne 2.3.1 lahendus

head(iris)

#1
iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length, decreasing = F),]

#2
iris.sort2[30,]
#"virginica"

###%Ülesanne 3.1.1

B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]

str(B)

#1
install.packages("reshape2")
library(reshape2)

#2
testid.pikk <- melt(B,measure.vars = 3:40, id.vars = 1 )

#3
str(testid.pikk)

###%Ülesanne 3.2.1 lahendus
#rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
#                  colClasses = c("numeric", "character", "character", "factor"))

#1
library(reshape2)

#2
rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var = "weight")

#3
rotid.lai

###%Ülesanne 3.3.1 lahendus
summary(rotid)

#1
tabel1 <- table(rotid$Rat, rotid$Time )
tabel1

#2
tabel2 <- dcast(rotid, formula = Rat ~ "mootmisi", fun.aggregate = length(rotid$weight))
tabel2

#3
#3 katkestajat

###%Ülesanne 3.4.1 lahendus

#1
tabel3 <- dcast(rotid, formula = Diet + Rat ~ "kaalu mediaan",fun.aggregate = mean, na.rm = TRUE, value.var = "weight")
tabel3

#2
rott2mediaan <- tabel3[2, 3]

###%Ülesanne 3.5.1 lahendus
arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)
#1
arstiabita

#2
pikk <- melt(arstiabita, variable.name = "Aasta")
pikk

#3
transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)  
transponeeritud

###%Ülesanne 3.6.1 lahendus

#1
transponeeritud <- recast(arstiabita, formula = variable ~ Arstiabiliik)
transponeeritud
