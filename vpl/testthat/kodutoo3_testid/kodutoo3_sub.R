###% --------- R kodutöö nr. 3 ---------


# ----- 1. Faktor-tüüpi tunnuse kasutamine -----

# ---- 1.1 Faktori tasemete järjestamine ----

# Vaata esmalt andmestikust ülevaadet:

summary(iris)


# 1.1.1 ÜLESANDED

# Ülesande 1 lahendus

keskmised <- by(iris$Petal.Length, INDICES = iris$Species, FUN = mean)
keskmised

# Ülesande 2 lahendus

# "setosa"

# Ülesande 3 lahendus

iris$sordinimi <- factor(iris$Species, 
                         levels = c("versicolor", "setosa", "virginica"))

# Ülesande 4 lahendus

maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, FUN = max)
maksimumid


###% ---- 1.2 Faktor-tunnuse loomine arvtunnusest ----


# 1.2.1 ÜLESANDED

# Ülesande 1 lahendus

intervallid <- cut(iris$Petal.Length, 
                   c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7),
                   right = F)

# Ülesande 2 lahendus

is.factor(intervallid)

# Ülesande 3 lahendus

sagedustabel <- table(intervallid)

# Ülesande 4 lahendus

tyhjad <- sum(sagedustabel[1:12] == 0)


###% ----- 2. Andmestiku sorteerimine -----

# ---- 2.1 Sorteerimine ----

x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)

# Vaata vektorite x, y ja andmestiku xy sisu:

x; y; xy


# 2.1.1 ÜLESANDED

# Ülesande 1 lahendus

xy1 <- xy[order(x, y), ]
xy1

# Ülesande 2 lahendus

xy2 <- xy[order(x, -y), ]
xy2

# Ülesande 3 lahendus

# Esimeses andmestikus on sorteeritud y-i järgi kasvavalt, teises andmestikus on y-i järgi kahanevalt.


###% ---- 2.2 Andmestiku sorteerimine kasvavalt ----

# Vaata andmestikku:

head(iris)


# 2.2.1 ÜLESANDED

# Ülesande 1 lahendus

iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width), ]

# Ülesande 2 lahendus

tail(iris.sort1)

# "setosa"


###% ---- 2.3 Andmestiku sorteerimine ----

# Vaata andmestikku:

head(iris)


# 2.3.1 ÜLESANDED

# Ülesande 1 lahendus

iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length), ]

# Ülesande 2 lahendus

iris.sort2[30, ]

# "virginica"


###% ----- 3. Andmestiku kuju teisendused -----

# ---- 3.1 Andmestiku viimine pikka formaati ----

B <-  read.csv2(file = "https://github.com/Rkursus/mooc/raw/main/data/B.csv",   
                nrows = 160, stringsAsFactors = T)
B <- B[ , substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]

# Vaata andmestiku kirjeldust:

str(B)


# 3.1.1 ÜLESANDED

# Ülesande 1 lahendus

library(reshape2)

# Ülesande 2 lahendus

testid.pikk <- melt(B, measure.vars = 3:42, id.vars = 1)

# Ülesande 3 lahendus

str(testid.pikk)



###% ---- 3.2 Andmestiku viimine laia formaati ----

# file.choose() avab akna, kus saad andmestiku 'rotid.csv' üles otsida:

#rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
#                  colClasses = c("numeric", "character", "character", "factor"))

# Vaata andmestikku:

summary(rotid)


# 3.2.1 ÜLESANDED

# Ülesande 1 lahendus

library(reshape2)

# Ülesande 2 lahendus

rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var = "weight")

# Ülesande 3 lahendus

rotid.lai


###% ---- 3.3 Andmestiku agregeerimine 1 ----

# Vaata andmestikku:

summary(rotid)


# 3.3.1 ÜLESANDED

# Ülesande 1 lahendus

tabel1 <- table(rotid$Rat)
tabel1

# Ülesande 2 lahendus

tabel2 <- dcast(rotid, formula = Rat + Diet ~ "mootmisi", 
                fun.aggregate = length, value.var = "Rat")
tabel2

# Ülesande 3 lahendus

# 3 rotti on enne katse lõppu katkestanud.


###% ---- 3.4 Andmestiku agregeerimine 2 ----


# 3.4.1 ÜLESANDED

# Ülesande 1 lahendus

tabel3 <- dcast(rotid, formula = Diet + Rat ~ "kaalu mediaan",
                fun.aggregate = median, value.var = "weight")
tabel3

# Ülesande 2 lahendus

rott2mediaan <- tabel3[tabel3$Rat == "no02", "kaalu mediaan"]


###% ---- 3.5 Tabeli pööramine ----

arstiabita <- read.table("https://github.com/Rkursus/mooc/raw/main/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)


# 3.5.1 ÜLESANDED

# Ülesande 1 lahendus

arstiabita

# Ülesande 2 lahendus

pikk <- melt(arstiabita, variable.name = "Aasta")
pikk

# Ülesande 3 lahendus

transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud


###% ---- 3.6 Tabeli pööramine ühe sammuga ----

?recast


# 3.6.1 ÜLESANDED

# Ülesande 1 lahendus

transponeeritud <- recast(arstiabita, formula = variable ~ Arstiabiliik)
transponeeritud
