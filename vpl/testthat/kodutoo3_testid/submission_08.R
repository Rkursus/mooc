###%Ülesanne 1.1.1 lahendus
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()

summary(iris)


# 1.
keskmised <- by(iris$Petal.Length, iris$Species, mean)
keskmised

# 2.
# "Setosa"

# 3.
iris$sordinimi <- factor(iris$Species, levels=c("versicolor", "setosa", "virginica"))
iris

# 4.
maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, max)
maksimumid



###%Ülesanne 1.2.1 lahendus
# 1.
intervallid <- cut(iris$Petal.Length, breaks=c(seq(1,7,0.5)), right = FALSE)
intervallid

# 2.
is.factor(intervallid)

# 3.
sagedustabel <- table(intervallid)
sagedustabel

# 4.
tyhjad <- 2



x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)

###%Ülesanne 2.1.1 lahendus
x; y; xy
# 1. 
xy1 <- xy[order(x,y), ]
xy1

# 2.
xy2 <- xy[order(x,-y), ]
xy2

# 3.
# Erinevus seisneb selles, et xy1-s on nii x-i kui ka y-i väärtused järjestatud kasvavalt,
# aga xy2-s on y-i väärtused järjestatud tagurpidi ehk kahanevalt (iga x-i väärtuse lõikes).



###%Ülesanne 2.2.1 lahendus
head(iris)

# 1.
iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width), ]
iris.sort1


# 2.
tail(iris) # Sorteerimata andmestikus on eelviimasel kohal virginica sordist lill.
tail(iris.sort1) # Sorteeritud andmestikus on eelviimasel kohal setosa sordist lill.



head(iris)
###%Ülesanne 2.3.1 lahendus
# 1.
iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length), ]
iris.sort2

# 2. 
iris.sort2[30, "Species"]
# 30. lille sort on virginica.


B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]


###%Ülesanne 3.1.1 lahendus
str(B)


# 1.
library(reshape2)

# 2.
testid.pikk <- melt(B, measure.vars = 3:42, id.vars = 1)
testid.pikk

# 3.
str(testid.pikk)




###%Ülesanne 3.2.1 lahendus
# file.choose() avab akna, kus saad andmestiku 'rotid.csv' üles otsida
#rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
#                  colClasses = c("numeric", "character", "character", "factor"))
head(rotid)
summary(rotid)

# 1.
library(reshape2)

# 2.
rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var = "weight")
rotid.lai


###%Ülesanne 3.3.1 lahendus
summary(rotid)


# 1.
tabel1 <- table(rotid$Rat)
tabel1

# 2.
tabel2 <- dcast(rotid, formula = Rat ~ "mootmisi", value.var = "Rat", fun.aggregate = length)
tabel2

# 3.
tabel2[ which(tabel2$mootmisi!=11) ,]
# Katkestanud on 3 rotti.


###%Ülesanne 3.4.1 lahendus
# 1.

tabel3 <- dcast(rotid, formula = Diet + Rat ~ "kaalu mediaan", value.var = "weight", fun.aggregate = median)
tabel3

# 2.
rott2mediaan <- tabel3[tabel3$Rat =="no02", "kaalu mediaan"]


###%Ülesanne 3.5.1 lahendus
library(reshape2)
arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)

# 1.
arstiabita

# 2.
pikk <- melt(arstiabita, variable.name = "Aasta", id.vars = "Arstiabiliik")
pikk

# 3.
transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud


###%Ülesanne 3.6.1 lahendus
?recast
transponeeritud <- recast(arstiabita, formula = variable ~ Arstiabiliik, id.var = "Arstiabiliik")
transponeeritud
