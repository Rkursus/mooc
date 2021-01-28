# II kodutöö

# Setting up workspace, install 'rstudioapi' if using for first time
#install.packages('rstudioapi')
#setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))


###% ylesanne 1.1.1 lahendus

#1
andmed2 <- read.table("https://raw.githubusercontent.com/Rkursus/sygis2019/master/data/tulemused.txt", header = T, sep = "\\", dec = ",")
print(andmed2)

#2
summary(andmed2)


###% ylesannne 1.2.1 lahendus
?read.csv

#1
#Õige on väide number 3


###% ylesanne 1.3.1 lahendus

#download.file("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", "A.csv",  mode = "wb")

#1
read.csv("A.csv", header = T, sep = ";",argumendinimi)
argumendinimi <- "nrows"

#2
list.files()

#3
andmed4 <- read.csv2("A.csv", nrows = 45)

#4
valik <- andmed4[1:10, c(-4, -5)]
valik


###% ylesanne 1.4.1 lahendus
download.file("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", "B.csv",  mode = "wb")
andmed5a <- read.csv2("B.csv")

#1
dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5])

#2
andmed5 <- read.csv2(file = "B.csv", nrows = 160, colClasses = "character")

#3
ontaust <- substr(names(andmed5), 1, 79) == "taust"
onhinnang <- substr(names(andmed5), 1, 79) == "hinnang"

valik <- andmed5[, ontaust | onhinnang]
str(valik)                  #Mingil põhjusel ei tööta


###% ylesanne 2.1.1 lahendus

#1
pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)

#2
esimene <- pojad$l1
pea <- mean(pojad$l1)
filter <- esimene > pea
filter  

#3
pojad1 <- pojad[filter, ]
dim(pojad1)


###% ylesanne 2.2.1 lahendus 

#1
kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
summary(kapsad)

#2
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"

#3
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"
kapsad1 <- kapsad[filter1 & filter2, ]

#4
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"
kapsad2 <- kapsad[filter1 | filter2, ]

#5
dim(kapsad1)
dim(kapsad2)
 #Vaatlusi on rohkem objektis kapsad2


###% ylesanne 2.3.1 lahendus

summary(pojad)

#1
pikkus_vahe <- pojad$l1 - pojad$l2
pojad <- cbind(pojad, pikkus_vahe)

#2
laius_suhe <- pojad$l1 / pojad$l2
pojad <- cbind(pojad, laius_suhe)

#3
uus <- pojad[, c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")]


###% ylesanne 2.4.1 lahendus

dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)

#1
summary(dieet)

#2
dieet$kaal1[250]=85

#3
dieet$kaal2[145] = NA


###% ylesanne 2.5.1 lahendus

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

#1
olemasBmitteA <- setdiff(B$id, A$id)

#2
AjaB <- union(A$id, B$id)
sort(AjaB, decreasing = T)

#3
uuring1 <- merge(A, B, by = "id", all = T)

#4
kesk <- mean(uuring1$test101, na.rm = T)

stand <- sd(uuring1$test101, na.rm = T)
stand

#5
uuring2 <- merge(A, B, by = "id", all.y = F, all.x = F)

#6
uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))
      #Mingil põhjusel ei tööta, sellest tulenevalt ka järgnev kood mitte

#7
tabel1 <- table(uuring2$grupp, uuring2$sugu)
tabel1

#8
tabel2 <- table(uuring2$sugu, uuring2$grupp)
tabel2

#9
filter_grupp <- uuring2$grupp == "c"
filter_sugu <- uuring2$sugu == 0
c.naisi <- uuring2[filter_grupp & filter_sugu, ]

