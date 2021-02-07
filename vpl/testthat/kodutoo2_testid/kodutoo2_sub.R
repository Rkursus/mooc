###% --------- R kodutöö nr. 2 ---------


# ----- 1. Andmete R-i toimetamine -----

# ---- 1.1 Andmete import 1 ----


# 1.1.1 ÜLESANDED

# Ülesande 1 lahendus

andmed2 <- read.table("https://github.com/Rkursus/mooc/raw/main/data/tulemused.txt",
                         header = T, sep = "\\", dec = ",")
andmed2

# Ülesande 2 lahendus

summary(andmed2)


###% ---- 1.2 Andmete import 2 ----

?read.csv


# 1.2.1 ÜLESANDED

# Ülesande 1 lahendus

# Õige on 3.väide.


###% ---- 1.3 Andmete import 3 ----

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#download.file("https://github.com/Rkursus/mooc/raw/main/data/A.csv", "A.csv",  mode = "wb")


# 1.3.1 ÜLESANDED

# Ülesande 1 lahendus

?read.csv
?read.csv2

argumendinimi <- "nrows"

# Ülesande 2 lahendus

list.files()

# Ülesande 3 lahendus

andmed4 <- read.csv2("A.csv", nrows = 45)

# Ülesande 4 lahendus

valik <- andmed4[1:10, -4:-5]
valik


###% ---- 1.4 Andmete import 4 ----

download.file("https://github.com/Rkursus/mooc/raw/main/data/B.csv", 
              "B.csv", mode = "wb")

andmed5a <- read.csv2("B.csv")


# 1.4.1 ÜLESANDED

# Ülesande 1 lahendus

dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5])

# Ülesande 2 lahendus

andmed5 <- read.csv2(file = "B.csv", nrows = 160, stringsAsFactors = FALSE)
andmed5

# Ülesande 3 lahendus

valik <- andmed5[ , startsWith(names(andmed5), c("hinnang", "taust"))]


# ----- 2. Toimingud andmestikuga -----

###% ---- 2.1 Andmete filtreerimine tingimuse põhjal ----


# 2.1.1 ÜLESANDED

# Ülesande 1 lahendus

pojad <- read.table("https://github.com/Rkursus/mooc/raw/main/data/pojad.txt", 
                    header = T)
pojad

# Ülesande 2 lahendus

filter <- pojad$l1 > mean(pojad$l1)
filter

# Ülesande 3 lahendus

pojad1 <- pojad[filter, ]
dim(pojad1)


###% ---- 2.2 Andmete filtreerimine, tingimuste kombineerimine ----


# 2.2.1 ÜLESANDED

# Ülesande 1 lahendus

kapsad <- read.table("https://github.com/Rkursus/mooc/raw/main/data/cabbages.txt", 
                     header = T)

summary(kapsad)

# Ülesande 2 lahendus

filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"

# Ülesande 3 lahendus

kapsad1 <- kapsad[filter1 & filter2, ]

# Ülesande 4 lahendus

kapsad2 <- kapsad[filter1 | filter2, ]

# Ülesande 5 lahendus

dim(kapsad1)
dim(kapsad2)

# Suurema objektide arvuga andmetabeli nimi on kapsad2.


###% ---- 2.3 Tunnuste lisamine andmestikku ----

summary(pojad)


# 2.3.1 ÜLESANDED

# Ülesande 1 lahendus

pojad$pikkus_vahe <- pojad$l1 - pojad$l2

# Ülesande 2 lahendus

pojad$laius_suhe <- pojad$b1/pojad$b2

# Ülesande 3 lahendus

uus <- pojad[ , c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")]


###% ---- 2.4 Paranda viga andmestikus ----

dieet <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/dieet.csv", header = T)


# 2.4.1 ÜLESANDED

# Ülesande 1 lahendus

summary(dieet)

# Ülesande 2 lahendus

dieet[which.max(dieet$kaal1), "kaal1"] <- dieet[which.max(dieet$kaal1), "kaal1"]/1000

# Ülesande 3 lahendus

dieet[dieet$kaal2 == 350, "kaal2"] <- is.na(dieet[dieet$kaal2 == 350, "kaal2"])


###% ---- 2.5 Hulgatehted ja andmestike ühendamine ----

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)
B <- B[ , c("id", "grupp", sort(names(B)[-(1:2)]))]


# 2.5.1 ÜLESANDED

# Ülesande 1 lahendus

olemasBmitteA <- setdiff(B$id, A$id)

# Ülesande 2 lahendus
  
AjaB <- sort(intersect(A$id, B$id), decreasing = TRUE)
  
# Ülesande 3 lahendus

uuring1 <- merge(A, B, by = "id", all = TRUE) 
  
# Ülesande 4 lahendus

kesk <- mean(uuring1$test101, na.rm = TRUE)

stand <- sd(uuring1$test101, na.rm = TRUE)

# Ülesande 5 lahendus

uuring2 <- merge(A, B, by = "id", all = FALSE)

# Ülesande 6 lahendus

uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))

# Ülesande 7 lahendus

tabel1 <- table(uuring2$grupp, uuring2$sugu2)
tabel1

# Ülesande 8 lahendus

tabel2 <-  prop.table(tabel1, margin = 1)
tabel2

# Ülesande 9 lahendus

c.naisi <- tabel1[3, 1]
c.naisi
