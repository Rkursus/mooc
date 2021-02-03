
###% Ülesanne 2.1.1 lahendus
# 1. Vastus: 25.30556
25 - 1/4 + 5/9

# 2. Vastus: 1.14641
(sqrt(3) + 4)/5

# 3. Vastus: 234256
(245 - 3^6)^2

# 4. Vastus: 0.09270204
(log(3) + 4)/55



###% Ülesanne 2.2.1 lahendus
# 1.
w <- 3
z <- w + 5
z



###% Ülesanne 2.3.1 lahendus
# 1. 
z <- 25*pi

# 2. Vastus: 1.89509
log(z, base=10)

# 3. Vastus: 4.363606
log(z)

# 4. Vastus: 60.9992
z + 1/z - 2^(z/19)



###% Ülesanne 2.4.1 lahendus
poisse <- 3
tydrukuid <- 2
lapsi <- poisse + tydrukuid
lapsi



###% Ülesanne 2.5.1 lahendus
jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")            
temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)

names(temp) <- jaam

# 1.
Fahrenheit <- temp * (9/5) + 32
Fahrenheit

# 2.
lisa <- c(-24.9, -16.1)
lisa
names(lisa) <- c("Mustvee", "Keila")
lisa

# 3.
temp2 <- c(temp, lisa)
temp2



###% Ülesanne 2.6.1 lahendus
# 1.
exp(temp2)
vastus <- "jah"

# 2.
summary(temp2)
vastus2 <- -16.10

# 3.
sd(temp2)
vastus3 <- "ei"



###% Ülesanne 2.7.1 lahendus
# 1.
temp; jaam

# Näide 1. Elementide valimine indeksite kaudu
temp[ 1 ] 
temp[ -1 ]
temp[ c(1, 5, 9) ] 

# Näide 2. Tulemuseks tõeväärtustega vektorid 
temp < -15  
jaam == "Tallinn" 

# Näide 3. Tingimustele vastavate elementide väljavalimine. Tõeväärtusvektori kasutamine
jaam[ temp < -15 ] 
temp[jaam == "Tallinn"]  

# 2.

vastus1 <- temp[ c(seq(2,length(temp), by=2))]
vastus1


# 3.
vastus2 <- jaam[ temp <= -17]
vastus2



###% Ülesanne 2.8.1 lahendus
muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 <- c(1:3, NA, 0, Inf - Inf)
muutuja3 <- 1:6
muutuja1; muutuja2; muutuja3
# 1.
is.logical(muutuja1)

# 2.
is.nan(muutuja2)

# 3.
is.na(muutuja3)[1] <- "TRUE"
muutuja3



###% Ülesanne 2.9.1 lahendus

x <- c(34, 23, 45, 67, 10, 21, 37)
x1 <- x > 50
x1

x2 <- x < 20
x2

x3 <- x1 | x2
x3

# 1.
x4 <- x > 30
x4

# 2.
x5 <- x < 40
x5

# 3.
x6 <- x4 & x5
x6



###% Ülesanne 2.10.1 lahendus

abc <- letters[1:3]
abc
abc == "a" | abc == "b" 
abc != "c"  
!(abc == "c")  

kordused <- c(45, 20, 68, 9)
y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))

table(y)

# 1.
viimane <- y=="tere" | y=="tsau"
viimane
