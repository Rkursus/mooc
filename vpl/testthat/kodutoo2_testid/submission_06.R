###%Ålesanne 1.1.1 lahendused

andmed2 <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/tulemused.txt", header = T, ,sep = "\\")
summary(andmed2)

###%Ålesanne 1.2.1 lahendused
#install.packages("rstudioapi")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Variant 1 on åige

###%Ålesanne 1.3.1.1 lahendused
argumendinimi <- "nrows"

#Ålesanne 1.3.1.2 lahendused
list.files()

#Ålesanne 1.3.1.3 lahendused
andmed4 <- read.csv("A.csv",header = T, sep = ";",nrows = 45)


#Ålesanne 1.3.1.4 lahendused
valik <- andmed4[1:10,c(1,2,3,6,7,8)]
valik
andmed5a <- read.csv2("B.csv")

###%Ålesanne 1.4.1.1 
dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5])

#Ålesamme 1.4.1.2 
andmed5 <- read.csv2(file = "B.csv", header = T, stringsAsFactors = F)


#Ålesanne 1.4.1.3
library(dplyr)
valik <- select(andmed5, matches("hinnang|taust"))

###%Ålesanne 2.1.1.1
pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)

#Ålesanne 2.1.1.2 
pere1_keskmine <- (colMeans(pojad,na.rm = T)[1] + colMeans(pojad,na.rm = T)[3])/2
filter <- c(pojad$l1 > pere1_keskmine)
print(filter)

#Ålesanne 2.1.1.3 
pojad1 <- pojad[filter,]
dim(pojad1)

###%Ålesanne 2.2.1.1
kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
summary(kapsad)

#Ålesanne 2.2.1.2 
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"

#Ålesanne 2.2.1.3
kapsad1 <- kapsad[filter1 & filter2,]

#Ålesanne 2.2.1.4
kapsad2 <- kapsad[filter1 | filter2,]

#Ålesanne 2.2.1.5 
#kapsad2 andmestikus on rohkem vaatlusi

###%Ålesanne 2.3.1.1
pojad$pikkus_vahe <- pojad$l1 - pojad$l2

#Ålesanne 2.3.1.2
pojad$laius_suhe <- pojad$b1 / pojad$b2

#Ålesanne 2.3.1.3
uus <- pojad[ ,c(1,3,5,2,4,6)]

###%Ålesanne 2.4.1.1 lahendus
dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)
summary(dieet)

#Ålesanne 2.4.1.2 lahendus
dieet[which.max(dieet$kaal1), 3] <- max(dieet$kaal1)/1000

#Ålesanne 2.4.1.3 lahendus
dieet[which.max(dieet$kaal2),4] <- " "

###%Ålesanne 2.5.1.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

olemasBmitteA <- setdiff(B$id,A$id)

#Ålesanne 2.5.1.2 lahendus
AjaB <- sort(intersect(A$id,B$id),decreasing = TRUE)

#Ålesanne 2.5.1.3 
uuring1 <- merge(A,B, by = "id",all.x = TRUE,all.y = TRUE)

#Ålesanne 2.5.1.4
kesk <- mean(uuring1$test101,na.rm = T)
stand <- sd(uuring1$test101,na.rm = T)

#Ålesanne 2.5.1.5
uuritavad <- c("sugu","elukoht", "vanus", "kasv", "kaal", "sirutus", "visiit", "grupp" )
uuring2 <- merge(A,B, by = "id")

#Ålesanne 2.5.1.6 
uuring2$sugu2 <- ifelse(uuring2$sugu == 1,"Mees","Naine")

#Ålesanne 2.5.1.7
tabel1 <- table(uuring2$sugu2)

#Ålesanne 2.5.1.8
tabel2 <- prop.table(tabel1)

#Ålesanne 2.5.1.9
c.naisi <- sum(table(uuring1$sugu)[1])
