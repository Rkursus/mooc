###% Ülesanne 2.1.1.1 lahendus
25-1/4+5/9

#Ülesanne 2.1.1.2 lahendus
(sqrt(3)+4)/5

#Ülesanne 2.1.1.3 lahendus
(245-3**6)**2

#Ülesanne 2.1.1.4 lahendus
(log(3)+4)/5


###%Ülesanne 2.2.1.1 lahendus
w <- 3
z <- w+5
print(z)

###% Ülesanne 2.3.1.1 lahendus
z <- 25*pi

#Ülesanne 2.3.1.2 lahendus
print(log10(z))

#Ülesanne 2.3.1.3 lahendus
print(log(z))

#Ülesanne 2.3.1.4 lahendus
print(z+1/z-2**(z/19))

###% Ülesanne 2.4.1 lahendus
poisse <- "kolm"
tydrukuid <- 2
tostring(tydrukuid)
lapsi <- paste(poisse, tydrukuid, sep="")

###% Ülesanne 2.5.1.1 lahendus
jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva") 
temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
Fahrenheit <- temp*9/5+32
print(Fahrenheit)

#Ülesanne 2.5.1.2 lahendus
lisa <- c(-24.9, -16.1)
names(lisa) <- c("Mustvee", "Keila")
print(lisa)

#Ülesanne 2.5.1.3 lahendus
temp2 <- c(temp, lisa)
print(temp2)

###% Ülesanne 2.6.1.1 lahendus
print(temp2)
exp(temp2)
vastus1 <- "Jah"

#Ülesanne 2.6.1.2 lahendus
summary(temp2)
vastus2 <- "-16.1"

#Ülesanne 2.6.1.3 lahendus
sd(temp2)
vastus3 <- "ei"

###% Ülesanne 2.7.1.1 lahendus
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

#Ülesanne 2.7.1.2 lahendus
vastus1 <- temp[c(seq(2, length(temp), by=2))]
print(vastus1)


#Ülesanne 2.7.1.3 lahendus
vastus2 <- jaam[temp<=17]
print(vastus2)

###% Ülesanne 2.8.1.1 lahendus
muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 <- c(1:3, NA, 0, Inf - Inf)
muutuja3 <- 1:6
muutuja1; muutuja2; muutuja3
is.logical(muutuja1)

#Ülesanne 2.8.1.2 lahendus
is.nan(muutuja2)

#Ülesanne 2.8.1.3 lahendus
muutuja3[1] <- " "
print(muutuja3)

###% Ülesanne 2.9.1.1 lahendus
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

x4 <- x>=30
x4

#Ülesanne 2.9.1.2 lahendus
x5 <- x<=40
x5

#Ülesanne 2.9.1.3 lahendus
x6 <- x4 & x5
x6

###% Ülesanne 2.10.1.1 lahendus
abc <- letters[1:3]
abc
abc == "a" | abc == "b" # täht on 'a' või täht on 'b'
abc != "c"  # täht ei ole 'c'
!(abc == "c")  # eitame väidet, et täht on 'c'
table(y)
kordused <- c(45, 20, 68, 9)
y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))
y

y1 <- y=="tsau"
y1
y2<- y=="tere"
viimane <- y1 | y2
viimane
