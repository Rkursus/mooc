###%Ülesanne 1.1.1 lahendus
summary(iris)

keskmised = by(iris$Petal.Length, iris$Species, mean, na.rm=TRUE)
keskmised

#"setosa"

iris$sordinimi = factor(iris$Species, levels = c("versicolor", "setosa", "virginica"))

maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, max, na.rm=TRUE)
maksimumid

###%Ülesanne 1.2.1 lahendus
intervallid <- cut(iris$Petal.Length, breaks = seq(1, 7, 0.5), right = F)

is.factor(intervallid)

sagedustabel <- table(intervallid)
sagedustabel

tyhjad <- 2
 
###%Ülesanne 2.1.1 lahendus
x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)

x; y; xy

xy1 <- xy[order(x, y), ]
xy1

xy2 <- xy[order(x, -y), ]
xy2

#Andmestikud on x järgi järjestatud samamoodi, aga erinevus seisneb selles,
# et kui x väärtused langevad kokku, siis nende järjestus on määratud y väärtuste
# põhjal, xy1 puhul on y väärtused järjestatud kasvavalt, xy2 puhul kahanevalt.

###%Ülesanne 2.2.1 lahendus
head(iris)

iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width), ]

tail(iris.sort1)
#"setosa"

###%Ülesanne 2.3.1 lahendus
head(iris)

iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length), ]

iris.sort2[30, ]
# "virginica"

###%Ülesanne 3.1.1 lahendus
B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]
str(B)

library(reshape2)

testid.pikk <- melt(B, id.vars = "id", measure.vars = colnames(B[, c(substr(names(B), 1, 4) %in% "test")]))

str(testid.pikk)

###%Ülesanne 3.2.1 lahendus
rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
                  colClasses = c("numeric", "character", "character", "factor"))
summary(rotid)

library(reshape2)

rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var = "weight")

rotid.lai

###%Ülesanne 3.3.1 lahendus
summary(rotid)

tabel1 <- table(rotid$Rat)
tabel1

tabel2 <- dcast(rotid, formula = Rat ~ "mootmisi")
tabel2

#Katkestajaid on 3.

###%Ülesanne 3.4.1 lahendus
tabel3 <- dcast(rotid, formula = Rat + Diet ~ "kaalu mediaan", value.var = "weight", fun.aggregate = median)
tabel3

rott2mediaan <- 240

###%Ülesanne 3.5.1 lahendus
arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)
arstiabita

pikk <- melt(arstiabita, variable.name = "Aasta")
pikk

transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud

###%Ülesanne 3.6.1 lahendus
transponeeritud <- recast(arstiabita, variable ~ Arstiabiliik)
transponeeritud