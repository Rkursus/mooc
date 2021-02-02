###% Lahendus: 1.1.1 Ülesanded-----------------------------------------
#1
andmed2 <- read.table("https://raw.githubusercontent.com/Rkursus/sygis2019/master/data/tulemused.txt", sep = "\\", header = T, dec = ",")
andmed2
#2
summary(andmed2)
###% Lahendus: 1.2.1 Ülesanded-----------------------------------------
?read.csv
#3 is correct
#getwd()
#---------------
#install.packages('rstudioapi')
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()
#-----------------
###% Lahendus: 1.3.1 Ülesanded-----------------------------------------

# Ülesanne 1: Omista muutujale selle argumendi nimi, mis määrab imporditava ridade arvu
argumendinimi <- "nrows"

# Ülesanne 2: kontrolli faili olemasolu töökaustas
list.files()

# Ülesanne 3: täienda antud koodi
andmed4 <- read.csv2("A.csv",nrows = 45)


# Ülesanne 4: prindi ekraanile nõutud alamosa andmestikust
valik <- andmed4[c(1:10),(-4:-5)]
valik

###% Lahendus: 1.4.1 Ülesanded-----------------------------------------
download.file("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", "B.csv",  mode = "wb")
andmed5a <- read.csv2("B.csv")
# Ülesanne 1: Pane kirja sobivad funktsiooninimed, ning viimases käsus sobivad indeksid
dim(andmed5a)
str(andmed5a)
tail(andmed5a[ ,1:5 ])


# Ülesanne 2: tee antud koodi vajalik täiendus, prindi tulemus ekraanile
andmed5 <- read.csv2(file = "B.csv", stringsAsFactors = FALSE)


# Ülesanne 3: vali andmestikust nõutud alamosa
valik <- andmed5[, substr(names(andmed5), 1, 7) == "hinnang" | substr(names(andmed5), 1, 5) == "taust"]
#View(valik)

###% Lahendus: 2.1.1 Ülesanded-----------------------------------------

# Ülesanne 1: prindi andmestik ekraanile
pojad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/pojad.txt", header = T)
pojad

# Ülesanne 2: moodusta tõeväärtusvektor
filter <- pojad$l1>mean(pojad$l1)
filter


# Ülesanne 3: vali tingimusele vastavad read andmestikust, küsi andmestiku dimensioone
pojad1 <- pojad[filter,]
dim(pojad1)

###% Lahendus: 2.2.1 Ülesanded-----------------------------------------

# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga
kapsad <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
summary(kapsad)

# Ülesanne 2: moodusta tõeväärtusvektorid
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"


# Ülesanne 3: vali tingimusele vastavad read andmestikust 
kapsad1 <- kapsad[filter1 & filter2,]


# Ülesanne 4:  vali tingimusele vastavad read andmestikust 
kapsad2 <- kapsad[filter1 | filter2,]


# Ülesanne 5:  pane kirja suurema objektide arvuga andmetabeli nimi
kapsad2

###% Lahendus: 2.3.1 Ülesanded-----------------------------------------

# Vaata meeldetuletuseks andmestikku:
summary(pojad)

# Ülesanne 1: uue tunnuse lisamine, vahe
pojad[,"pikkus_vahe"] <- pojad$l1-pojad$l2


# Ülesanne 2: uue tunnuse lisamine, suhe
pojad[,"laus_suhe"] <- pojad$b1/pojad$b2

# Ülesanne 3: andmestiku veergude ümberjärjestamine
uus <- pojad[,c(1,3,5,2,4,6)]

###% Lahendus: 2.4.1 Ülesanded-----------------------------------------

# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga

dieet <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/dieet.csv", header = T)
summary(dieet)

# Ülesanne 2: Tee grammides kaalule teisendus kilodeks
max(dieet$kaal1)
max(dieet$kaal2)
grammrow <- which.max(dieet$kaal1)
dieet[grammrow, 3] <- dieet[grammrow,3]/1000

# Ülesanne 3: Asenda vigane väärtus tühikuga

max(dieet$kaal1)
max(dieet$kaal2)
wrongrow <- which.max(dieet$kaal2)
dieet[wrongrow, 4] <- NA

###% Lahendus: 2.5.1 Ülesanded-----------------------------------------

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

# Ülesanne 1: leia isikud, kes on andmestikus B, kuid mitte A-s
olemasBmitteA <- setdiff(B$id, A$id)


# Ülesanne 2: leia isikud, kes on mõlemas andmestikus
AjaB <- sort(intersect(B$id, A$id), decreasing = TRUE)


# Ülesanne 3: Liida anmdestikud, tulemuses olgu kõik isikud mõlemast andmestikust.
uuring1 <- merge(A,B, by = "id", all = TRUE)


# Ülesanne 4: leia nõutud keskväärtus ...
kesk <- mean(uuring1$test101, na.rm = TRUE)
# ... ja standardhälve
stand <- sd(uuring1$test101, na.rm = TRUE)

# Ülesanne 5: Liida andmestikud, tulemuses olgu kõik isikud kel on mõlemas andmestikus info olemas.
uuring2 <- merge(A,B, by = "id", all = FALSE)

# Ülesanne 6: faktortunnuse loomine
uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine","Mees"))

# Ülesanne 7: Sagedustebel
(tabel1 <- table(uuring2$grupp, uuring2$sugu2))


# Ülesanne 8: Jaotustabel
(tabel2 <-  prop.table(tabel1, margin = 1))


# Ülesanne 9: Naiste arv grupis c
c.naisi <- tabel1["c","Naine"]











