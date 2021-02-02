###%Ülesanne 1.1.1 lahendus

#1

andmed2 <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/tulemused.txt", 
           header = T, sep = "\\", dec=",")
andmed2
#2

summary(andmed2)

###%Ülesanne 1.2.1 lahendus

#1

#Õige: 3

###%Ülesanne 1.3.1 lahendus

#1

argumendinimi <- "nrows"

#2 

list.files()

#3

andmed4 <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
andmed4

#4

valik <- andmed4[1:10 , c(-4, -5)]
valik

###%Ülesanne 1.4.1 lahendus

#download.file("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", "B.csv",  mode = "wb")
andmed5a <- read.csv2("B.csv")
andmed5a

#1

dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5])

#2

andmed5 <- read.csv2(file = "B.csv", stringsAsFactors = F)

#3

valik <- andmed5[, substr("hinnang" | "taust", 1, 1 )]

###%Ülesanne 2.1.1 lahendus

#1

pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)
pojad

#2

filter <- c(pojad$l1 > mean(pojad$l1))
filter

#3

pojad1 <- pojad[filter, ]
dim(pojad1)

###%Ülesanne 2.2.1 lahendus

kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)

#1

summary(kapsad)

#2

filter1 <- c(kapsad$Cult == "c52")
filter2 <- c(kapsad$Date == "d21")

#3

kapsad1 <- kapsad[c(filter1 & filter2), ]
kapsad1

#4

kapsad2 <- kapsad[c(filter1 | filter2), ]
kapsad2

#5

kapsad2

###%Ülesanne 2.3.1 lahendus

#1

pojad[, "pikkus_vahe"] <- pojad$l1 - pojad$l2

#2 

pojad[, "laus_suhe"] <- pojad$b1 / pojad$b2

#3

uus <- pojad[, c("l1", "l2", "pikkus_vahe", "b1", "b2", "laus_suhe")]

###%Ülesanne 2.4.1 lahendus

dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)

#1

summary(dieet)

#2

which.max(dieet$kaal1)
dieet[250, 3] <- 85

#3

dieet[145, 4] <- NA
is.na(dieet[145, 4]) <- " "

###%Ülesanne 2.5.1 lahendus

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]


#1 vahe

olemasBmitteA <- setdiff(B$id, A$id)

#2 ühisosa

AjaB <- sort(intersect(A$id, B$id), decreasing = T)

#3 ühend

uuring1 <- merge(A, B, by = "id", all = T)

#4 

#keskväärtus
kesk <- mean(uuring1$test101, na.rm = T)

#hälve
stand <- sd(uuring1$test101, na.rm = T)

#5

uuring2 <- merge(A, B, by = "id")

#6

uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))

#7

tabel1 <- table(uuring2$grupp, uuring2$sugu2)

#8

tabel2 <- prop.table(tabel1)
tabel2
# 9

c.naisi <- 1


