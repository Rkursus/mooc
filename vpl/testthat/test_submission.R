###% ylesanne2.1.1 ----
#1.
25-1/4+5/9

#2.
(sqrt(3)+4)/5

#3.
(245-3^6)^2

#4.
(log(3)+4)/55




###% ylesanne2.2.1. ----
#1.
w <- 3
z <- w+5
z



###% ylesanne2.3.1. ----
#1.
z = 25*pi

#2.
log(z, 10)

#3.
log(z)

#4.
z + 1/z - 2^(z/19)



###% ylesanne2.4.1. ----
poisse <- 3
tydrukuid <- 2
lapsi <- poisse + tydrukuid



###% ylesanne2.5.1. ----
temp <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
jaam <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")            
names(temp) <- jaam
temp

#1.
Fahrenheit <- temp * 9/5 + 32
Fahrenheit

#2.
lisa <- c(-24.9, -16.1)
lisa
names(lisa) <- c("Mustvee", "Keila")

#3.
temp2 <- c(temp, lisa)
temp2



###% ylesanne2.6.1. ----
#1.
exp(temp2)
vastus1 <- "jah"

#2.
summary(temp2)
vastus2 <- -16.1


#3.
sd(temp2)
vastus3 <- "ei"



###% ylesanne2.7.1. ----
#1.
temp; jaam
temp[1]
temp[-1]
temp[c(1,5,9)]
temp < -15
jaam == "Tallinn"
jaam[ temp < -15 ]
temp[jaam == "Tallinn"]

#2.
vastus1 <- temp[seq(2, 8, by=2)]
vastus1

#3.
vastus2 <- jaam[temp <= -17]
vastus2



###% ylesanne2.8.1. ----
muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
muutuja2 <- c(1:3, NA, 0, Inf - Inf)
muutuja3 <- 1:6
muutuja1; muutuja2; muutuja3

#1.
is.logical(muutuja1)

#2.
is.nan(muutuja2)

#3.
is.na(muutuja3)[1] <- TRUE
muutuja3



###% ylesanne2.9.1. ----
x <- c(34, 23, 45, 67, 10, 21, 37)
x1 <- x > 50
x1
x2 <- x < 20
x2
x3 <- x1 | x2
x3

#1.
x4 <- x > 30
x4

#2.
x5 <- x < 40
x5

#3.
x6 <- x4 & x5
x6



###% ylesanne2.10.1. ----
abc <- letters[1:3]
abc
abc == "a" | abc == "b"
abc != "c"
!(abc == "c")
#table(y)
kordused <- c(45, 20, 68, 9)
y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))

# Ülesanne
viimane <- y == "tere" | y == "tsau"
viimane
table(y)
