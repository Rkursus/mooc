###% Ülesanne 2.1.1.1
 25 - 1 / 4 + 5 / 9  
   vastus <- 25.30556
 
# Ülesanne 2.1.1.2  
 (sqrt(3) + 4) / 5 
   vastus <- 1.14641
 
# Ülesanne 2.1.1.3  
 (245-3**6)**2 
   vastus <- 234256
   
# Ülesanne 2.1.1.4
 log(3) / 55 + 4 
   vastus <- 4.019975
 
# Näide 1. Omistame muutjale y väärtuse 2 ja väljastame väärtuse
 y <- 2
 
# Näide 2. Kasutame muutjat y arvutuses
 
 y + 5
 vastus <- 7
 
 ###% Ülesanne 2.2.1 
 
 w <- 3
 z <- w + 5 
 z <- 8
 
 ###% Ülesanne 2.3.1.1
 25 * pi
 z <- 78.53982
 
# Ülesanne 2.3.1.2
  log10(z) 
  vastus <- 1.89509
 
# Ülesanne 2.3.1.3 
  log(z) 
  vastus <- 4.363606
 
# Ülesanne 2.3.1.4
  z + 1 / z - 2 ** (z / 19) 
  vastus <- 60.9992

###% Ülesanne 2.4.1 
 
 poisse <- 3
 tydrukuid <- 2
 lapsi <- paste(poisse + tydrukuid) 
 vastus <- 5
 
###% Ülesanne 2.5.1 
 
 temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
 jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")
 
 temp; jaam
 names(temp) <- jaam
 temp
 
 # Ülesanne 2.5.1.1
 
 Fahrenheit <- temp * (9 / 5) + 32
 Fahrenheit <- c(20.84, 8.78, 8.60, 4.28, 3.02,  1.5,  1.40, -3.28, -3.82) 
 
# Ülesanne 2.5.1.2
 
 lisa <- c(-24.9, -16.1)
 jaam2 <-  c ("Mustvee", "Keila")
 lisa; jaam2
 names(lisa) <- jaam2
 lisa
 
 # Ülesanne 2.5.1.3
 
  temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
  lisa <- c(-24.9, -16.1)
  temp2 <-c(temp, lisa)
  
  temp2 <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9, -24.9, -16.1)
 
 
###% Ülesanne 2.6.1
 
  temp2 <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9, -24.9, -16.1)
  exp(temp2)
  vastus1 <- #jah
 
  temp2 < c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9, -24.9, -16.1)
  summary(temp2)
  vastus2 <- -16.10
  
  sd(temp2)
  vastus3 <- 4.731346
  
###% Ülesanne 2.7.1.1
 # Näide 1. Elementide valimine indeksite kaudu
  temp[ 1 ] # vektori esimene element
  temp[ -1 ] # vektori kõik elemendid va esimene
  temp[ c(1, 5, 9) ] # vektori esimene, viies ja üheksas element
  
  # Näide 2. Tulemuseks tõeväärtustega vektorid
  temp < -15
  
  jaam == "Tallinn"

  # Näide 3. Tingimustele vastavate elementide väljavalimine. Tõeväärtusvektori kasutamine
  
  jaam[ temp < -15 ] # valime välja need jaamad, kus temperatuur on alla -15
  temp[jaam == "Tallinn"]  # valime välja Tallinnale vastava temperatuuri
 
 # Ülesanne 2.7.1.2
  
  temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
  temp <- seq(from = 2, to = 8, by = 2)
  vastus1 <- temp[-12.9, -15.4, -16.9, -19.6]
  
  # Ülesanne 2.7.1.3
  
  jaam[ temp = <- 17 ] 
  vastus2 <- # Ruhnu, Kihnu, Pakri, Tallinn, Pärnu, Kunda, Kuusiku
  
###% Ülesanne 2.8.1.1
  
  muutuja1 <- c("TRUE", "true", "Tru", "FALSE", "F", "false", NA)
  muutuja2 <- c(1:3, NA, 0, Inf - Inf)
  muutuja3 <- 1:6
  muutuja1; muutuja2; muutuja3
  
    
  muutuja1 <- c("TRUE", "true", "Tru", "FALSE", "F", "false", NA)
  is.logical(muutuja1)  
  
  # Ülesanne 2.8.1.2
  
  muutuja2 <- c(1:3, NA, 0, Inf - Inf)
  is.nan(muutuja2)
  
  # Ülesanne 2.8.1.3
  
  muutuja3 <- 1:6
  is.na(muutuja3) [1] <- "FALSE"
  muutuja3
  muutuja3 <- c(1, 2, 3, 4, 5, 6, NA)
  
###% Ülesanne 2.9.1
  
  # antud on vektor x
  x <-  c(34, 23, 45, 67, 10, 21, 37)
  
  # Näide 1: Väärtustame tõeväärtusvektori, mille elementide väärtus on TRUE,
  # kui x väärtused on suuremad kui 50.
  
  x1 <- x > 50
  x1 
  
  # Näide 2: Väärtustame tõeväärtusvektori, mille elementide väärtus on TRUE,
  # kui x väärtused on väiksemad kui 20.
  x2 <- x < 20
  x2
  
  
# Näide 3: Moodustame tõeväärtusvektori, mis näitab, millised x väärtused on
  # alla 20 või üle 50.
  x3 <- x1 | x2
  x3
  
# Ülesanne 2.9.1.1
  
  x4 <- x > 30
  x4

# Ülesanne 2.9.1.2
  
  x5 <- x < 40
  x5

# Ülesanne 2.9.1.3
  
  x6 <- x4 | x5
  x6
  
  
###% Ülesanne 2.10
  
# Näide: vaatame tähestiku algust, moodustame kolme moodi tõevektori, mille väärtus on TRUE,
# kui täht on a või b ning on FALSE vastasel korral
  
  abc <- letters[1:3]
  abc
  abc == "a" | abc == "b" # täht on "a" või täht on "b"
  abc != "c" # täht ei ole "c"
  ! (abc == "c") # eitame väidet, et täht on "c"

  table(y)

  kordused <- c(45, 20, 68, 9)
  y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))

  y == "tere"
  y == "tsau"
 
  viimane <- c(45, 68)
  
    
  
  
  
 
 