###% Lahendus: 2.1.1 Ülesanded--------------------------------------------------------------------

#1:
# I am not quite sure if you mean with 1:4 the "array" or the simple division
# I am assuming it is the division since it's the very first exercise
# division done with "/", no brackets needed
25-1/4+5/9
#------------------------

#2:
(sqrt(3)+4)/5
#------------------------

#3:
(245-3^6)^2
#------------------------

#4:
# I am not quite sure if you mean ln(3)+4 or ln(3+4), but i'm going with log(3)+4
(log(3, base = exp(1))+4)/55

###% Lahendus: 2.2.1 Ülesanne--------------------------------------------------------------------
# Näide 1. Omistame muutjale y väärtuse 2 ja väljastame väärtuse
y <- 2
y

# Näide 2. Kasutame muutujat y arvutuses
y + 5

w <- 3
z <- (w+5)
z

###% Lahendus: 2.3.1 Ülesanded--------------------------------------------------------------------

#1:
z <- 25*pi
#------------------------

#2:
log10(z)
#------------------------

#3:
log(z)
#------------------------

#4:
z+(1/z)-2^(z/19)


###% Lahendus: 2.4.1 Ülesanded--------------------------------------------------------------------

#You can only add 2 numbers. Since poisse is a text variable, we need to change poisse
# to a numerical variable. We just substitute now the word for the number

poisse <- 3
tydrukuid <- 2
lapsi <- poisse + tydrukuid
lapsi

###% Lahendus: 2.5.1 Ülesanded--------------------------------------------------------------------

temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva") 
names(temp) <- jaam
temp

# Ülesanne 1: Teisenda temperatuurid celsiuse skaalalt fahrenheiti skaalale 
#(asenda alakriips vajaliku tehtega) ja prindi tulemus ekraanile

Fahrenheit <- temp * (9/5) + 32 
Fahrenheit 
#------------------------

# Ülesanne 2. Prindi ekraanile vektor nimega 'lisa'
lisa <- c(-24.9, -16.1)
lisa
names(lisa) <- c("Mustvee","Keila")
lisa
#------------------------


# Ülesanne 3. Moodusta nõutud kujul uus vektor, selleks asenda alakriipsud vajalike objektidega. Prindi tulemus ekraanile.
temp2 <- c(temp, lisa) 
temp2


###% Lahendus: 2.6.1 Ülesanne--------------------------------------------------------------------


# Ülesanne 1: Rakenda funktsiooni exp() ja pane kirja vastus (asenda alakriips sobiva koodiga)
exp(temp2)
vastus1 <- "jah"
#------------------------

# Ülesanne 2: Rakenda funktsiooni summary() ja pane kirja vastus (asenda alakriips sobiva koodiga)
summary(temp2)
vastus2 <- -16.10
#------------------------

# Ülesanne 3: Rakenda funktsiooni sd() ja pane kirja vastus (asenda alakriips sobiva koodiga)
sd(temp2)
vastus3 <- "ei"
#sd >= 0

###% Lahendus: 2.7.1 Ülesanne--------------------------------------------------------------------

# Objektid nimedega temp ja jaam on töölaual juba olemas
temp; jaam

# Näide 1. Elementide valimine indeksite kaudu
temp[ 1 ] # vektori esimene element
temp[ -1 ] # vektori kõik elemendid va esimene
temp[ c(1, 5, 9) ] # vektori esimene, viies ja üheksas element

# Näide 2. Tulemuseks tõeväärtustega vektorid 
temp < -15 # Kontrollime millised temperatuurid jäävad alla -15 kraadi 
jaam == "Tallinn"  # Millisel kohal vektoris on ilmajaama nimi Tallinn?

# Näide 3. Tingimustele vastavate elementide väljavalimine. Tõeväärtusvektori kasutamine
jaam[ temp < -15 ] # valime välja need  jaamad, kus temperatuur on alla -15
temp[jaam == "Tallinn"]  # valime välja Tallinnale vastava temperatuuri
#------------------------

# Ülesanne 1: Vali nõutud elemendid temperatuurivektorist, omista tulemus muutujale vastus1. 
# Prindi tulemus ekraanile
vastus1 <- temp[seq(length(temp))%%2 == 0]
vastus1
#------------------------

# Ülesanne 2: Vali välja tingimusele vastavad ilmajaamade nimed, omista tulemus muutujale vastus2. 
# Prindi tulemus ekraanile
vastus2 <- jaam[temp<=-17]
vastus2

###% Lahendus: 2.8.1 Ülesanne--------------------------------------------------------------------

muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 <- c(1:3, NA, 0, Inf - Inf)
muutuja3 <- 1:6
muutuja1; muutuja2; muutuja3

# Ülesanne 1: Kontrolli kas vektor muutuja1 on tõeväärtusvektor 
# (asenda alakriipis sobivalt, et moodustuks õige funktsiooni nimi).
is.logical(muutuja1)
#------------------------

# Ülesanne 2: Rakenda funktsiooni is.nan() vektorile muutuja2.

is.nan(muutuja2)

#------------------------

# Ülesanne 3: Asenda vektori muutuja3 esimene element puuduva väärtusega, selleks asenda 
# järgmises käsus alakriipis sobiva tõeväärtusega. Prindi tulemus ekraanile.
is.na(muutuja3)[1] <- TRUE
muutuja3

###% Lahendus: 2.9.1 Ülesanne--------------------------------------------------------------------

# antud on vektor x
x <- c(34, 23, 45, 67, 10, 21, 37)

#Näide 1: Väärtustame tõeväärtusvektori, mille elementide väärtus on `TRUE`, 
# kui x väärtused on suuremad kui 50. Prindime ekraanile
x1 <- x > 50
x1

#Näide 2: Väärtustame tõeväärtusvektori, mille elementide väärtus on `TRUE`, 
# kui  x väärtused on  väiksemad kui 20. Prindime ekraanile
x2 <- x < 20
x2

#Näide 3: Moodustame tõeväärtusvektori, mis näitab, millised x väärtused on 
# alla 20 või üle 50. Prindime ekraanile
x3 <- x1 | x2 # loogiline tehe 'või'
x3
#------------------------

#Ülesanne 1: Väärtusta tõeväärtusvektor x4. Prindi x4 ekraanile
x4 <- x > 30
x4
#------------------------

#Ülesanne 2: Moodusta tõeväärtusvektor vektor x5. Prindi  ekraanile
x5 <- x < 40
x5
#------------------------

#Ülesanne 3: Moodusta tõeväärtusvektor x6 kasutades vektoreid x4 ja x5 
# ning sobivat loogilist tehet. Prindi tulemus ekraanile
x6 <- x4 & x5
x6

###% Lahendus: 2.10.1 Ülesanne--------------------------------------------------------------------

# Näide: vaatame tähestiku algust, moodustame kolme moodi tõevektori, mille väärtus on TRUE, 
# kui täht on a või b ning on FALSE vastasel korral
abc <- letters[1:3]
abc

abc == "a" | abc == "b" # täht on 'a' või täht on 'b'

abc != "c"  # täht ei ole 'c'

!(abc == "c")  # eitame väidet, et täht on 'c'


# Ülesandes on uurimise all tekstiväärtustega vektor y, vaata selle väärtuste sagedustabelit
table(y)


# Jooksuta järgnev kood
kordused <- c(45, 20, 68, 9)
y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))

viimane <- y == "tsau" | y == "tere"
viimane






