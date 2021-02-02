###% Lahendus: 1.1.1 Ülesanded-----------------------------------------

# Vaata esmalt andmestikust ülevaadet
#View(iris)
summary(iris)


# Ülesanne 1: Leia keskmised kroonlehepikkused käsu by abil
keskmised <- by(iris$Petal.Length, iris$Species, mean)
keskmised


# Ülesanne 2: Mis sordil keskmiselt kõige lühemad kroonlehed
#"setosa"


# Ülesanne 3: Muuda sordinimede järjestust, tekita selleks uus tunnus
iris$sordinimi <- factor(iris$Species, levels = c("versicolor", "setosa", "virginica"), ordered = T)


# Ülesanne 4: Leia maksimaalne kroonlehe pikkus sortide kaupa, sordid olgu uues järjestuses
maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, max)
maksimumid

###% Lahendus: 1.2.1 Ülesanded-----------------------------------------

#Ülesanne 1 Jaga kroonlehtede pikkus intervallidesse
#I am not sure what you mean by:
#Ära unusta tekkivate faktoritasemete silte muuda.
#so I added the labels command
intervallid <- cut(iris$Petal.Length, breaks = seq(1,7,0.5), labels = c("1-1,4", "1,5-1,9", "2-2,4", "2,5-2,9", "3-3,4", "3,5-3,9", "4-4,4", "4,5-4,9", "5-5,4", "5,5-5,9", "6-6,4", "6,5-6,9"), right = F)


#Ülesanne 2: Kas vektor intervallid on faktor-tüüpi?
is.factor(intervallid)


#Ülesanne 3: Sagedustabel
sagedustabel <- table(intervallid)
sagedustabel


#Ülesanne 4: Mitu tühja intervalli tekkis?
tyhjad <- 2

###% Lahendus: 2.1.1 Ülesanded-----------------------------------------

x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)

# Vaata vektorite x, y ja andmestiku xy sisu
x; y; xy

# Ülesanne 1: andmestiku sorteerimine kasvavalt
xy1 <- xy[order(x,y), ]
xy1


# Ülesanne 2: ridade järjestus order(x, -y) põhjal
xy2 <- xy[order(x,-y), ]
xy2


# Ülesanne 3: Milles seisneb erinevus? (vastust kirjuta kommentaarina)
# sorting first after x, and if x is the same for 2 values, it will look at y
# x is still in the same order (increasing) as before, but y is not increasing anymore but decreasing

###% Lahendus: 2.2.1 Ülesanded-----------------------------------------

# vaata andmestikku
head(iris)

# Ülesanne 1: sorteerimine
iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Width),  ]

# ülesanne 2: eelviimase lille sort
# "setosa"

###% Lahendus: 2.3.1 Ülesanded-----------------------------------------

# vaata andmestikku
head(iris)

# Ülesanne 1: sorteerimine
iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length), ]

# ülesanne 2: kolmekümnenda lille sort
# "virginica"

###% Lahendus: 3.1.1 Ülesanded-----------------------------------------

B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]

# Vaata andmestiku kirjeldust
str(B)

# Ülesanne 1: aktiveeri lisapakett
library(reshape2)


# Ülesanne 2: vii andmestik pikale kujule
testid.pikk <- melt(B, measure.vars = 3:42, id.vars = 1)


# Ülesanne 3: vaata andmestiku struktuuri
str(testid.pikk)

###% Lahendus: 3.2.1 Ülesanded-----------------------------------------

# file.choose() avab akna, kus saad andmestiku 'rotid.csv' üles otsida
rotid <- read.csv(file = "https://raw.githubusercontent.com/Rkursus/sygis2019/master/data/rotid.csv", header = T, stringsAsFactors = F, 
                  colClasses = c("numeric", "character", "character", "factor"))

# vaata andmestikku
summary(rotid)

# Ülesanne 1: aktiveeri lisapakett
library(reshape2)


# Ülesanne 2: vii andmestik laiale kujule
rotid.lai <- dcast(rotid, formula = Rat + Diet ~ Time, value.var = "weight")

# Ülesanne 3: prindi uus andmestik ekraanile
rotid.lai

###% Lahendus: 3.3.1 Ülesanded-----------------------------------------

# vaata andmestikku
summary(rotid)

# Ülesanne 1: Leia sagedustabel
tabel1 <- table(rotid$Rat)
tabel1


# ÜLesanne 2: Leia samad näitajad käsu dcast abil
tabel2 <- dcast(rotid, Rat ~ "mootmisi")
tabel2


# Ülesanne 3: Kui palju on katkestajaid
# 3 rats

###% Lahendus: 3.4.1 Ülesanded-----------------------------------------

# Ülesanne 1: Leia mediaanide tabel dcast käsuga
tabel3 <- dcast(rotid, formula = Rat + Diet ~ "kaalu mediaan", fun.aggregate = median, value.var = "weight")
tabel3


# Ülesanne 2: roti nr 2 kaalu mediaan
rott2mediaan <- 240

###% Lahendus: 3.5.1 Ülesanded-----------------------------------------

arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)


# Ülesanne 1: prindi andmestik ekraanile
arstiabita

# Ülesanne 2: vii andmestik pikale kujule, vaata tulemust
pikk <- melt(arstiabita, variable.name = "Aasta")
pikk

# Ülesanne 3: teisenda pikk andmestik laiaks nii, et tulemuseks on pööratud andmestik. Prindi tulemus ekraanile
transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud

###% Lahendus: 3.6.1 Ülesanded-----------------------------------------

# Ülesanne 1: pööra andmestik. Prindi tulemus ekraanile
transponeeritud <- recast(arstiabita, variable ~ Arstiabiliik)
transponeeritud



