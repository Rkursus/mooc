###%Ülesanne 1.1.1 lahendus
andmed2 = read.table("https://github.com/Rkursus/sygis2019/raw/master/data/tulemused.txt", 
                     header = T, sep = "\\", dec = ",")                     
andmed2

summary(andmed2)

###%Ülesanne 1.2.1 lahendus
#3. on õige väide

###%Ülesanne 1.3.1 lahendus
download.file("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", "A.csv",  mode = "wb")
argumendinimi = "nrows"

list.files()

andmed4 = read.csv2("A.csv", nrows = 45)

valik = andmed4[c(1:10), c(-4, -5)]
valik

###%Ülesanne 1.4.1
download.file("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", "B.csv",  mode = "wb")
andmed5a <- read.csv2("B.csv")
dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5 ])

andmed5 <- read.csv2(file = "B.csv", nrows = 160, stringsAsFactors = F)
andmed5

###%Ülesanne 2.1.1
pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)
pojad

filter = pojad$l1>mean(pojad$l1)
filter

pojad1 = pojad[filter,]
dim(pojad1)

###%Ülesanne 2.2.1 lahendus
kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
summary(kapsad)

filter1 = kapsad$Cult=="c52"
filter2 = kapsad$Date=="d21"

kapsad1 = kapsad[filter1 & filter2,]

kapsad2 = kapsad[filter1 | filter2,]

#Rohkem vaatlusi on andmestikus kapsad2.

###%Ülesanne 2.3.1 lahendus
summary(pojad)

pojad$pikkus_vahe = pojad$l1-pojad$l2

pojad$laius_suhe = pojad$b1/pojad$b2

uus = pojad[, c(1, 3, 5, 2, 4, 6)]

###%Ülesanne 2.4.1 lahendus
dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)
summary(dieet)

dieet[which.max(dieet$kaal1), 3] = dieet[which.max(dieet$kaal1), 3]/1000

is.na(dieet[dieet$kaal2==350, 4]) = T

###%Ülesanne 2.5.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

olemasBmitteA = setdiff(B,A)

AjaB = sort(intersect(A, B), decreasing = T)

uuring1 = merge(A, B, all = T)

kesk = mean(uuring1$test101, na.rm=T)
stand = sd(uuring1$test101, na.rm=T)

uuring2 = merge(A, B)

uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))

tabel1 <- table(uuring2$grupp, uuring2$sugu2)
tabel1

c.naisi <- tabel1[3, "Naine"]
c.naisi