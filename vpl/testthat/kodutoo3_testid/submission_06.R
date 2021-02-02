###% Ülesanne 1.1.1.1 lahendus

keskmised <- by(iris, iris$Species, function(x){
  mean.pl <- mean(x$Petal.Length)
})
keskmised

#Ülesanne 1.1.1.2 lahendus
#"Keskmiselt on kõige lühemad kroonlehed sordil setosa"

#Ülesanne 1.1.1.3 lahendus
iris$sordinimi <- factor(c("versicolor", "setosa", "virginica"))

#Ülesanne 1.1.1.4 lahendus
maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, max)
maksimumid

###%Ülesanne 1.2.1.1 lahendus
intervallid <- cut(iris$Petal.Length, seq(1, 7, by=0.5), labels = paste("[", seq(1, 6.5, by = 0.5), "-", seq(1.5, 7, by = 0.5), ")"), include.lowest = T)
intervallid

#Ülesanne 1.2.1.2 lahendus
is.factor(intervallid)

#Ülesanne 1.2.1.3 lahendus
sagedustabel <- table(intervallid)
sagedustabel

#Ülesanne 1.2.1.4 lahendus
tyhjad <- sum(sagedustabel==0)
tyhjad

###%Ülesanne 2.1.1.1 lahendus
x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)
xy1 <- xy[order(x,y),]
xy1

#Ülesanne 2.1.1.2 lahendus
xy2 <- xy[order(x, -y),]
xy2

#Ülesanne 2.1.1.3 lahendus
#"Esimeses järjestatakse y kasvavalt, teises kahanevalt."

###%Ülesanne 2.2.1.1 lahendus
head(iris)
iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width),]

#Ülesanne 2.2.1.2 lahendus
tail(iris.sort1, n=2)
#"Eelviimase lille sort on setosa (sordnimi versicolor), leidsin eelnimetatud funktsiooniga, vaatasin saadud tabeli esimest väärtust

###%Ülesanne 2.3.1.1 lahendus
head(iris)
iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length),]
iris.sort2

#Ülesanne 2.3.1.2 lahendus
iris.sort2[30,]

###%Ülesanne 3.1.1.1 lahendus
B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]
str(B)
install.packages("reshape2")
library(reshape2)

#Ülesanne 3.1.1.2 lahendus
testid.pikk <- melt(B, id.vars=1, measure.vars=3:42)

#Ülesanne 3.1.1.3 lahendus
str(testid.pikk)

###%Ülesanne 3.2.1.1 lahendus
rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
                  colClasses = c("numeric", "character", "character", "factor"))
library(reshape2)

#Ülesanne 3.2.1.2 lahendus
rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var="weight")

#Ülesanne 3.2.1.3 lahendus
rotid.lai

###%Ülesanne 3.3.1.1 lahendus
summary(rotid)
tabel1 <- table(rotid$Rat)
tabel1

#Ülesanne 3.3.1.2 lahendus
tabel2 <- dcast(rotid, formula = Rat ~ "mootmisi")
#"Katse katkestas kolm rotti"

###%Ülesanne 3.4.1.1
tabel3 <- dcast(rotid, formula = Rat + Diet ~ "kaalu mediaan", value.var = "weight", fun.aggregate = median)
tabel3

#Ülesanne 3.4.1.2 
rott2mediaan <- tabel3[3, 3]
rott2mediaan

###%Ülesanne 3.5.1.1
arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)
arstiabita

#Ülesanne 3.5.1.2 
pikk <- melt(arstiabita, variable.name = "Aasta")

#Ülesanne 3.5.1.3
transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud

###%Ülesanne 3.6.1.1
transponeeritud1 <- recast(arstiabita, variable~Arstiabiliik, id.var = 1)
transponeeritud1
