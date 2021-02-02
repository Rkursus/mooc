###% Kodutöö nr 2

andmed2 <- read.table("https://raw.githubusercontent.com/Rkursus/sygis2019/master/data/tulemused.txt", 
                      header = T, sep = "\\", dec = ",")
summary(andmed2)

?read.csv

###% Ülesanne 1.2.1 vastus
# õige on väide nr 3

#download.file("https://raw.githubusercontent.com/Rkursus/sygis2019/master/data/A.csv", 
              "A.csv", mode = "wb")
#install.packages("rstudioapi")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###% Ülesanne 1.3.1.1

argumendinimi <- "nrows"

###% Ülesanne 1.3.1.2

list.files(path = ".")

###% Ülesanne 1.3.1.3

andmed4 <- read.csv2("A.csv", header = TRUE, sep = ";", na.rm = "TRUE")

###% Ülesanne 1.3.1.4
  
valik <- andmed4[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(-4, -5)]
valik
  

###% 1.4. Andmete import
  
download.file("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", "B.csv", mode = "wb")
andmed5a <- read.csv2("B.csv")

# Ülesanne 1.4.1.1

dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , c(1:5)])

# Ülesanne 1.4.1.2 

andmed5 <- read.csv2(file = "B.csv", character(length = 0))
  
# Ülesanne 1.4.1.3
  
valik <- substr("andmed5", "hinnang", "taust")
  
###% Ülesanne 2.1.1.1
  
pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)

# Ülesanne 2.1.1.2 

mean(pojad$l1)
filter <- mean(pojad$l1) < "l1"
filter

# Ülesanne 2.1.1.3

pojad1 <- pojad[mean(pojad$l1) < "l1", ]
dim(pojad1)


###% Ülesanne 2.2.1.1

kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
summary(kapsad)

# Ülesanne 2.2.1.2

filter1 <- kapsad == "c52"
filter1
filter2 <- kapsad == "d21"
filter2

# Ülesanne 2.2.1.3

kapsad1 <- c(filter1, filter2)
kapsad1  

# Ülesanne 2.2.1.4

kapsad2 <- c(filter1 | filter2)
kapsad2
  
# Ülesanne 2.2.1.5 

kapsad1
  

###% Ülesanne 2.3.1.1

summary(pojad)

pojad$pikkus_vahe <- pojad$l1 - pojad$l2

# Ülesanne 2.3.1.2

pojad$laius_suhe <- pojad$b1 / pojad$b2

# Ülesanne 2.3.1.3 

uus <- pojad[, c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")]


###% Ülesanne 2.4.1.1

dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)
summary(dieet)

# Ülesanne 2.4.1.2 

dieet[which.max(dieet$kaal1), ]
dieet$kaal1[250] <- 85

# Ülesanne 2.4.1.3 

dieet[350, "kaal2"] = ""


###% Ülesanne 2.5.1.1

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

olemasBmitteA <- setdiff(B$id, A$id)
olemasBmitteA
  
# Ülesanne 2.5.1.2
  
AjaB <- sort(intersect(B$id, A$id), decreasing = TRUE)
AjaB

# Ülesanne 2.5.1.3
  
uuring1 <- merge(B, A, by = "id", all = TRUE)
uuring1

# Ülesanne 2.5.1.4

kesk <- mean(uuring1$test101, na.rm = TRUE)
stand <- sd(uuring1$test101, na.rm = TRUE)
  
# Ülesanne 2.5.1.5
uuring2 <- merge(B, A, by = "id", na.rm = FALSE)
uuring2

# Ülesanne 2.5.2.6

uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))
uuring2$sugu2
  
# Ülesanne 2.5.2.7

tabel1 <- table(uuring2$grupp, uuring2$sugu2)
print(tabel1)

# Ülesanne 2.5.2.8

tabel2 <- tabel1
print(tabel2)

# Ülesanne 2.5.2.9

naised <- uuring2[uuring2$sugu == 0, ]

grupp.c <- uuring2[uuring2$grupp == c]

c.naisi <-
