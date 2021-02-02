###% 3. kodutöö R

iris

# Ülesanne 1.1.1.1

summary(iris)
keskmised <- by(iris$Petal.Length, list(iris$Species), mean)
keskmised

# Ülesanne 1.1.1.2

#"setosa"

# Ülesanne 1.1.1.3

iris$sordinimi <- factor((iris$Species), levels = c("versicolor", "setosa", "virginica"))

# Ülesanne 1.1.1.4

maksimumid <- tapply(iris$Petal.Length, iris$sordinimi, median)
maksimumid


###% Ülesanne 1.2.1.1

intervallid <- cut(iris$Petal.Length, 
                   breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7))


# Ülesanne 1.2.1.2

is.factor(intervallid)

# Ülesanne 1.2.1.3

sagedustabel <- table(intervallid)
sagedustabel

# Ülesanne 1.2.1.4  

tyhjad <- 2


###%Ülesanne 2.1.1.1

x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)

xy1 <- xy[order(x, y), ]
xy1

# Ülesanne 2.1.1.2

xy2 <- xy[order(x, -y), ]
xy2

# Ülesanne 2.1.1.3

# xy1 puhul on mõlemad väärtused kasvavas järjestuses, xy2 puhul on y väärtused ümberpööratud


###% Ülesanne 2.2.1.1

head(iris)

iris.sort1 <- iris[order(iris$Sepal.Width, iris$Sepal.Length, iris$Petal.Length), ]

# Ülesanne 2.2.1.2

tail(iris.sort1)

# "setosa"


###% Ülesanne 2.3.1.1

head(iris)

iris.sort2 <- iris[order(iris$Sepal.Width, -iris$Sepal.Length), ]


# Ülesanne 2.3.1.2 

# "setosa"


###% Ülesanne 3.1.1.1

B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",
                nrows = 160, stringsAsFactors = T)
B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]

str(B)

library(reshape2)
install.packages("reshape2")


# Ülesanne 3.1.1.2

testid.pikk <- melt(B, id.vars = "id", measure.vars = 3:40)

# Ülesanne 3.1.1.3

str(testid.pikk)


###% Ülesanne 3.2.1.1 

rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
                  colClasses = c("numeric", "character", "character", "factor"))

summary(rotid)

install.packages("reshape2")

# Ülesanne 3.2.1.2

rotid.lai <- dcast(rotid, formula = Rat + Diet ~ weight, value.var = "Diet", fun.aggregate = length)

# Ülesanne 3.2.1.3

print(rotid.lai)


###% Ülesanne 3.3.1.1

summary(rotid)

tabel1 <- table(rotid$Rat)                   
tabel1

# Ülesanne 3.3.1.2 

tabel2 <- dcast(rotid, formula = Rat  ~ "mootmisi", value.var = "Rat", fun.aggregate = length)
tabel2

# Ülesanne 3.3.1.3

# kolm


###% Ülesanne 3.4.1.1

tabel3 <- dcast(rotid, formula = Diet + Rat + median(weight) ~ "kaalu mediaan",
                value.var = "weight", fun.aggregate = length) 
tabel3

# Ülesanne 3.4.1.2

rott2mediaan <- 
  
  
###% Ülesanne 3.5.1.1
  
arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                           sep = "\t", dec = ",", header = T, check.names = FALSE)

arstiabita

# Ülesanne 3.5.1.2

pikk <- melt(arstiabita, measure.vars = 2:15, variable.name = "Aasta")
pikk

# Ülesanne 3.5.1.3

transponeeritud <- dcast(pikk, Aasta ~ Arstiabiliik)
transponeeritud


###% Ülesanne 3.6.1.1  

?recast

transponeeritud <- recast(arstiabita, variable ~ Arstiabiliik, measure.var = 2:15)
transponeeritud


