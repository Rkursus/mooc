###% 1. kodutöö

# Ülesanne 2.1.1 lahendus
#1
25-1/4+5/9
#2
(sqrt(3)+4)/5
#3
(245-3**6)**2
#4
(log(3)+4)/55

###% Ülesanne 2.2.1 lahendus

# Näide 1. Omistame muutjale y väärtuse 2 ja väljastame väärtuse
y <- 2
y

# Näide 2. Kasutame muutujat y arvutuses
y + 5

#lahenduse algus

w <- 3
z <- w + 5
z

###% Ülesanne 2.3.1 lahendus

z <- 25*pi
log10(z)
log(z)
z + 1/z - 2**(z/19)

###% Ülesanne 2.4.1 lahendus

# Näide 1: omistame muutujale x väärtuseks  teksti "Tere maailm!" ja väljastame selle
x <- "Tere maailm!"
x

# Näide 2: tekstide ühendamine, eri võimalusi
poisse <- "kolm"
tydrukuid <- 2
paste(poisse, "ja", tydrukuid)
paste(poisse, tydrukuid)
paste(poisse, tydrukuid, sep = "")

# Näide 3: tekste ei saa liita, tulemuseks on veateade.
poisse <- 3
tydrukuid <- 2
lapsi <- poisse + tydrukuid

###% Ülesanne 2.5.1 lahendus

#Näide 1: Moodustame 2 vektorit, millest ühes on kirjas temperatuurid (20.01.2010 kell 10), 
# teises ilmajaamad, kus need on mõõdetud :
temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")            

# Näide 2: Väljastame tulemused ekraanile
temp; jaam

# Näide 3: Paneme temperatuuridele jaamanimed juurde ja vaatame tulemust
names(temp) <- jaam
temp

#1
Fahrenheit <- temp*(9/5) + 32 
Fahrenheit 

#2
lisa <- c(-24.9, -16.1)
lisa
names(lisa) <- c("Mustvee", "Keila")

#3
temp2 <- c(temp, lisa) 
temp2

###% Ülesanne 2.6.1 lahendus

#1
exp(temp2)
vastus1 <- "jah"

#2
summary(temp2)
vastus2 <- -16.10

#3
sd(temp2)
vastus3 <- "ei"

###% Ülesanne 2.7.1 lahendus

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

#2
vastus1 <- temp[seq(from =2, to = 8, by = 2)]
vastus1

#3
vastus2 <- jaam[temp <= -17]
vastus2

###% Ülesanne 2.8.1 lahendus

# Moodustame mõned vektorid ja vaatame tulemust.
muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 <- c(1:3, NA, 0, Inf - Inf)
muutuja3 <- 1:6
muutuja1; muutuja2; muutuja3

#1
is.logical(muutuja1)

#2
is.nan(muutuja2)

#3
is.na(muutuja3) [1] <- TRUE
muutuja3

###% Ülesanne 2.9.1 lahendus

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

#1

x4 <- x > 30
x4

#2

x5 <- x < 40
x5

#3

x6 <- x4 | x5 
x6

###% Ülesanne 2.10.1 lahendus

# Näide: vaatame tähestiku algust, moodustame kolme moodi tõevektori, mille väärtus on TRUE, 
# kui täht on a või b ning on FALSE vastasel korral
abc <- letters[1:3]
abc
abc == "a" | abc == "b" # täht on 'a' või täht on 'b'
abc != "c"  # täht ei ole 'c'
!(abc == "c")  # eitame väidet, et täht on 'c'


# Ülesandes on uurimise all tekstiväärtustega vektor y, vaata selle väärtuste sagedustabelit
table(y)

# Näide: Jooksuta järgnev kood
kordused <- c(45, 20, 68, 9)
y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))

# Näide: lahenduse algus

viimane <- y == "tere" | y == "tsau"
viimane