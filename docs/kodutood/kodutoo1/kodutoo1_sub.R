###% --------- R kodutöö nr. 1 ---------

### ---- 2.1 Arvutamine ----


# 2.1.1 ÜLESANDED

# Ülesanne 1


# Ülesanne 2 


# Ülesanne 3


# Ülesanne 4



###% ---- 2.2 Muutujate kasutamine ----

# Näide 1. Omistame muutjale y väärtuse 2 ja väljastame väärtuse

y = 2
y

# Näide 2. Kasutame muutujat y arvutuses

y + 5


# 2.2.1 ÜLESANDED

# Ülesanne 1





###% ---- 2.3 Muutujate kasutamine tehetes ----


# 2.3.1 ÜLESANDED

# Ülesanne 1



# Ülesanne 2



# Ülesanne 3



# Ülesanne 4




###% ---- 2.4 Tekstiväärtusega muutujad ----

# Näide 1: omistame muutujale x väärtuseks  teksti "Tere maailm!" ja väljastame selle

x = "Tere maailm!"
x

# Näide 2: tekstide ühendamine, eri võimalusi

poisse = "kolm"
tydrukuid = 2
paste(poisse, "ja", tydrukuid)
paste(poisse, tydrukuid)
paste(poisse, tydrukuid, sep = "")

# Näide 3: tekste ei saa liita, tulemuseks on veateade

poisse = "kolm"
tydrukuid = 2
lapsi = poisse + tydrukuid


# 2.4.1 ÜLESANNE

# Ülesanne 1




###% ---- 2.5 Vektori moodustamine, tehted vektoriga ----
 
# Näide 1: Moodustame 2 vektorit, millest ühes on kirjas temperatuurid (20.01.2010 kell 10), 
# teises ilmajaamad, kus need on mõõdetud:

temp = c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
jaam = c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")            

# Näide 2: Väljastame tulemused ekraanile

temp; jaam

# Näide 3: Paneme temperatuuridele jaamanimed juurde ja vaatame tulemust

names(temp) = jaam
temp


# 2.5.1 ÜLESANDED

# Ülesanne 1

Fahrenheit = ________
Fahrenheit

# Ülesanne 2

lisa = ________
names(lisa) = ________


# Ülesanne 3  




###% ---- 2.6 Funktsioonide rakendamine vektoritele ----


# 2.6.1 ÜLESANDED

temp2

# Ülesanne 1 

________
vastus1 = ________

# Ülesanne 2 

________
vastus2 = ________

# Ülesanne 3

________
vastus3 = ________


###% ---- 2.7 Vektori alamosade selekteerimine ----


# 2.7.1 ÜLESANDED

# Ülesanne 1

# Kontrolli, kas objektid nimedega temp ja jaam on töölaual juba olemas
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

# Ülesanne 2

vastus1 = ________
vastus1

# Ülesanne 3

vastus2 = ________
vastus2


###% ---- 2.8 Väärtuste tüübid. Puuduvad väärtused ----

# Moodustame mõned vektorid ja vaatame tulemust.

muutuja1 = c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 = c(1:3, NA, 0, Inf - Inf)
muutuja3 = 1:6
muutuja1; muutuja2; muutuja3


# 2.8.1 ÜLESANDED

# Ülesanne 1



# Ülesanne 2



# Ülesanne 3




###% ---- 2.9 Veel tõeväärtustest 1 ----

# antud on vektor x
x = c(34, 23, 45, 67, 10, 21, 37)

#Näide 1: Väärtustame tõeväärtusvektori, mille elementide väärtus on `TRUE`, 
# kui x väärtused on suuremad kui 50. Prindime ekraanile

x1 = x > 50
x1

#Näide 2: Väärtustame tõeväärtusvektori, mille elementide väärtus on `TRUE`, 
# kui  x väärtused on  väiksemad kui 20. Prindime ekraanile

x2 = x < 20
x2

#Näide 3: Moodustame tõeväärtusvektori, mis näitab, millised x väärtused on 
# alla 20 või üle 50. Prindime ekraanile

x3 = x1 | x2 # loogiline tehe 'või'
x3


# 2.9.1 ÜLESANDED

# Ülesanne 1

x4 = ________
x4

# Ülesanne 2

x5 = ________
x5

# Ülesanne 3

x6 = ________
x6


###% ---- 2.10 Veel tõeväärtustest 2 ----

# Näide: vaatame tähestiku algust, moodustame kolme moodi tõevektori, mille väärtus on TRUE, 
# kui täht on a või b ning on FALSE vastasel korral

abc = letters[1:3]
abc
abc == "a" | abc == "b" # täht on 'a' või täht on 'b'
abc != "c"  # täht ei ole 'c'
!(abc == "c")  # eitame väidet, et täht on 'c'


# Ülesandes on uurimise all tekstiväärtustega vektor y, vaata selle väärtuste sagedustabelit

table(y)

# Jooksuta järgnev kood

kordused = c(45, 20, 68, 9)
y = sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))


# 2.10.1 ÜLESANNE

# Ülesanne 1

viimane = ________
viimane

