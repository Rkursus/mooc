###% --------- R kodutöö nr. 2 ---------


# ----- 1. Andmete R-i toimetamine -----

# ---- 1.1 Andmete import 1 ----


# 1.1.1 ÜLESANDED

# Ülesanne 1: Kasutades käsku read.table() impordi R-i fail tulemused.txt

andmed2 <- _________
andmed2

# Ülesanne 2: Vaata andmestiku tunnuste ülevaadet käsuga summary()





###% ---- 1.2 Andmete import 2 ----

?read.csv


# 1.2.1 ÜLESANDED

# Ülesanne 1: Milline neist väidetest on õige? (NB! omista vastus muutujale ‘vastus’).

# 1. Funktsiooni read.csv argumentide vaikeväärtused määravad, et imporditavas failis peab väljaeraldajaks olema semikoolon.
# 2. Funktsiooni read.csv ei saa kasutada .txt-laiendiga faili importimiseks.
# 3. Funktsiooni read.csv argumentide vaikeväärtused määravad, et imporditavas failis peab kümnendemurru eraldajaks olema punkt.
# 4. Funktsiooni read.csv argumentide vaikeväärtused määravad, et imporditavas failis peab kümnendemurru eraldajaks olema koma.


# Ülesanne 1: Vastus anna õige väite numbriga
vastus <- ____
vastus


###% ---- 1.3 Andmete import 3 ----

# Juhul kui sa pole 'rstudioapi' paketti veel R-i lisanud, siis kustuta
# alumise rea eest '#' märk ja käivita mõlemad käsud

#install.packages('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

download.file("https://github.com/Rkursus/mooc/raw/main/data/A.csv", "A.csv",  mode = "wb")

# 1.3.1 ÜLESANDED

# Ülesanne 1: Omista muutujale selle argumendi nimi, mis määrab imporditava ridade arvu
argumendinimi <- "_______________"


# Ülesanne 2: kontrolli faili olemasolu töökaustas



# Ülesanne 3: täienda antud koodi
andmed4 <- read.csv__("A.csv", ____________)


# Ülesanne 4: prindi ekraanile nõutud alamosa andmestikust
valik <- ______________
valik


###% ---- 1.4 Andmete import 4 ----

download.file("https://github.com/Rkursus/mooc/raw/main/data/B.csv", 
              "B.csv", mode = "wb")

andmed5a <- read.csv2("B.csv")


# 1.4.1 ÜLESANDED

# Ülesanne 1: Pane kirja sobivad funktsiooninimed, ning viimases käsus sobivad indeksid
_____(andmed5a)
_____(andmed5a)
tail(andmed5a[ , ___:___ ])


# Ülesanne 2: tee antud koodi vajalik täiendus, prindi tulemus ekraanile
andmed5 <- read.csv2(file = "B.csv", ________________)


# Ülesanne 3: vali andmestikust nõutud alamosa
valik <- __________________________





# ----- 2. Toimingud andmestikuga -----

###% ---- 2.1 Andmete filtreerimine tingimuse põhjal ----


# 2.1.1 ÜLESANDED

# Kasutame andmestikku pojad. Mõõdetud on perede esimese ja teise poja pea pikkus ja laius. 
#  Mõõtmised on millimeetrites. Tunnused andmestikus on järgmised:
#   l1 – esimese poja pea pikkus
#   l2 – teise poja pea pikkus
#   b1 – esimese poja pea laius
#   b2 – teise poja pea laius

pojad <- read.table("https://github.com/Rkursus/mooc/raw/main/data/pojad.txt", 
                    header = T)

# Ülesanne 1: prindi andmestik ekraanile




# Ülesanne 2: moodusta tõeväärtusvektor
filter <- ___________________________
filter


# Ülesanne 3: vali tingimusele vastavad read andmestikust, küsi andmestiku dimensioone
pojad1 <- ____________________
______(pojad1)


###% ---- 2.2 Andmete filtreerimine, tingimuste kombineerimine ----


# 2.2.1 ÜLESANDED

# Kasutame admestikku kapsad. Uuritud on kahe kapsasordi kapsapeade kaalu ja vitamiinide sisaldust. 
#  Tunnused andmestikus on järgmised:
#   Cult - kapsa sort
#   Date - külvikuupäev: 16., 20. või 21. kuupäev
#   HeadWt - kapsapea kaal, kg
#   VitC - kapsapea C-vitamiini sisaldus


kapsad <- read.table("https://github.com/Rkursus/mooc/raw/main/data/cabbages.txt", 
                     header = T)

# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga


# Ülesanne 2: moodusta tõeväärtusvektorid
filter1 <- ___________________________
filter2 <- ___________________________


# Ülesanne 3: vali tingimusele vastavad read andmestikust 
kapsad1 <- ____________________


# Ülesanne 4:  vali tingimusele vastavad read andmestikust 
kapsad2 <- ____________________


# Ülesanne 5:  pane kirja suurema objektide arvuga andmetabeli nimetuses olev number
vastus <- ____



###% ---- 2.3 Tunnuste lisamine andmestikku ----

# Kasutame andmestikku pojad. Meeldetuletuseks: 
#  l1 – esimese poja pea pikkus
#  l2 – teise poja pea pikkus
#  b1 – esimese poja pea laius
#  b2 – teise poja pea laius


# 2.3.1 ÜLESANDED

# Vaata meeldetuletuseks andmestikku:
summary(pojad)


# Ülesanne 1: uue tunnuse lisamine, vahe



# Ülesanne 2: uue tunnuse lisamine, suhe



# Ülesanne 3: andmestiku veergude ümberjärjestamine
uus <- pojad[, ____________]




###% ---- 2.4 Paranda viga andmestikus ----

# loe töölauale andmestik dieet. Tegu on teatavate dieetide mõju uuringuga. 
#  Uuringus osalejaid on kaalutud enne dieeti (kaal1) ja pärast dieeti (kaal2). 
#  Lisaks on teada iga inimese identifikaator ja dieedi tüüp.

dieet <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/dieet.csv", header = T)


# 2.4.1 ÜLESANDED

# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga




# Ülesanne 2: Tee grammides kaalule teisendus kilodeks




# Ülesanne 3: Asenda vigane väärtus puuduva väärtusega




###% ---- 2.5 Hulgatehted ja andmestike ühendamine ----


# Loe töölauale uuesti kaks varem kasutatud andmestikku:
#  andmestikus A on kirjas vastajate id-kood, sugu, elukoht, vanus, pikkus, kaal, käte siruulatus ning arstivisiidi toimumine
#  andmestikus B on kirjas vastajate id-kood, uuringugrupi tunnus ning vastused mitmesugustele testidele

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)
B <- B[ , c("id", "grupp", sort(names(B)[-(1:2)]))]


# 2.5.1 ÜLESANDED

# Ülesanne 1: leia isikud, kes on andmestikus B, kuid mitte A-s
olemasBmitteA <- ________________


# Ülesanne 2: leia isikud, kes on mõlemas andmestikus
AjaB <- ________________


# Ülesanne 3: Liida anmdestikud, tulemuses olgu kõik isikud mõlemast andmestikust.
uuring1 <- merge(_____________________)


# Ülesanne 4: leia nõutud keskväärtus ...
kesk <- ________________
# ... ja standardhälve
stand <- ______________


# Ülesanne 5: Liida andmestikud, tulemuses olgu kõik isikud kel on mõlemas andmestikus info olemas.
uuring2 <- merge(_____________________)


# Ülesanne 6: faktortunnuse loomine
uuring2$sugu2 <- _____(uuring2$sugu, labels = ________)


# Ülesanne 7: Sagedustebel
tabel1 <- table(_____, ______)


# Ülesanne 8: Jaotustabel
tabel2 <-  ______________


# Ülesanne 9: Naiste arv grupis c
c.naisi <- __________


