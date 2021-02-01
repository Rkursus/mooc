###% ylesanne 1.1.1 lahendus, III kodutöö

#1
summary(iris)
keskmised = by(iris$Petal.Length, iris$Species, mean)
keskmised

#2
#setosa

#3
iris$sordinimi = factor(iris$Species, levels= c("versicolor","setosa","virginica"))

#4
maksimumid = tapply(iris$Petal.Length, iris$sordinimi,max)
maksimumid

###% ylesanne 1.2.1 lahendus

#1
intervallid = cut(iris$Petal.Length,breaks = seq(1,7,0.5),right=FALSE)

#2
is.factor(intervallid)

#3
sagedustabel = table(intervallid)
sagedustabel

#4
tyhjad = 2

###% ylesanne 2.1.1 lahendus

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

###% ylesanne 2.2.1 lahendus

head(iris)

#1
iris.sort1 = iris[order(iris$Sepal.Width, iris$Sepal.Length,iris$Petal.Width),]

#2
iris.sort1$sordinimi[nrow(iris)-1]

###% ylesanne 2.3.1 lahendus

#1
iris.sort2 = iris[order(iris$Sepal.Width,-iris$Sepal.Length),]

#2
iris.sort2$sordinimi[30]

###% ylesanne 3.1.1 lahendus

B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]

#1
str(B)
library(reshape2)

#2
testid.pikk = melt(B,id.vars = 1, measure.vars = 3:42)

#3
str(testid.pikk)

###% ylesanne 3.2.1 lahendus

#rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
#                  colClasses = c("numeric", "character", "character", "factor"))
summary(rotid)
#1
library(reshape2)

#2
rotid.lai = dcast(rotid, Rat+Diet ~ Time, value.var = "weight")

#3
rotid.lai

###% ylesanne 3.3.1 lahendus

#1
tabel1 = table(rotid$Rat)
tabel1

#2
tabel2 = dcast(rotid, Rat~"mootmisi")
tabel2

#3
# 3

###% ylesanne 3.4.1 lahendus

#1
tabel3 = dcast(rotid, Diet+Rat~"kaalu mediaan",value.var = "weight", fun.aggregate = median)
tabel3
#2
rott2mediaan = 240.0

###% ylesanne 3.5.1 lahendus

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

###% ylesanne 3.6.1 lahendus

#1
transponeeritud = recast(data = arstiabita, formula = variable~Arstiabiliik)
transponeeritud
