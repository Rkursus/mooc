library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tstEnv$tmp_file = gsub(" ","", readLines(filename))

# Test setup
context("Kodutöö 1 kontroll")



# Ülesanne 2.1.1 õige lahendus -----

tstEnv$ylesanne = "Ülesanne 2.01.1"

test_that(tstEnv$ylesanne, 
          {
            # TO BE WRITTEN
          })

# Ülesanne 2.2.1 õige lahendus -----

tstEnv$ylesanne = "Ülesanne 2.02.1"

test_that(tstEnv$ylesanne, 
  {
    expect_equal(object = tstEnv$w,
                 expected = 3,
                 info = paste0(tstEnv$ylesanne, " Muutujale 'w' on antud vale väärtus"))
    
    expect_equal(object = tstEnv$z,
                 expected = 8,
                 info = paste0(tstEnv$ylesanne, " Muutuja 'z' on valesti arvutatud"))
    
    # expect_true(length(grep("read\\.table\\(.+(header.+sep.+dec|sep.+header.+dec|dec.+sep.+header|sep.+dec.+header).+\\)", tmp_file)) > 0, 
    #           info = paste0(ylesanne, " käsust on midagi puudu"))
    
    expect_true(length(grep("^z$", tstEnv$tmp_file)) > 0, 
                info = paste0(tstEnv$ylesanne, " muutujat z pole välja prinditud"))
    
  })

# Ülesanne 2.3.1 õige lahendus -----
if(FALSE){
  z=25*pi
  log10(z)
  log(z)
  z+(1/z)-2^(z/19)
}

tstEnv$ylesanne = "Ülesanne 2.03.1"

test_that(tstEnv$ylesanne, 
          {
            #1
            expect_equal(object = tstEnv$z,
                         expected = 25*pi,
                         info = paste0(tstEnv$ylesanne, " muutujale 'u' on antud vale väärtus"))
            
            #2
            expect_true(length(grep("log10\\(z\\)", tstEnv$tmp_file)) > 0 | length(grep("log\\(z,base=10\\)", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, " kümnendlogaritmi pole leitud või pole kasutatud muutujat 'u'"))
            
            #3
            expect_true(length(grep("log\\(z\\)", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, " naturaallogaritmi pole leitud või pole kasutatud muutujat 'u'"))
          
            #4
            expect_true(length(grep("z\\+\\(1\\/z\\)-2\\^\\(z\\/19\\)", tstEnv$tmp_file)) > 0 |       # juht z+(1/z)-2^(z/19)
                          length(grep("z\\+\\(1\\/z\\)-2\\**\\(z\\/19\\)", tstEnv$tmp_file)) > 0 |    # juht z+(1/z)-2**(z/19)
                            length(grep("z\\+1\\/z-2\\**\\(z\\/19\\)", tstEnv$tmp_file)) > 0 |        # juht z+1/z-2**(z/19)
                          length(grep("z\\+1\\/z-2\\^\\(z\\/19\\)", tstEnv$tmp_file)) > 0,            # juht z+1/z-2^(z/19)
                        info = paste0(tstEnv$ylesanne, " viimane tehe muutujaga z on valesti arvutatud"))
            
          })


# Ülesanne 2.4.1 õige lahendus -----
if(FALSE){
  # Näide 3: tekste ei saa liita, tulemuseks on veateade.
  poisse <- "kolm"
  tydrukuid <- 2
  lapsi <- poisse + tydrukuid
  
  # Ülesanne: Paranda näite 3 koodi nii, et liitmisel tuleks vastuseks arv ning punast veateadet ei ilmuks.
  poisse <- 3
  tydrukuid <- 2
  lapsi <- poisse + tydrukuid
}

tstEnv$ylesanne = "Ülesanne 2.04.1"

test_that(tstEnv$ylesanne, 
          {
            #1
            expect_is(tstEnv$poisse,"numeric")
            
            #2
            expect_equal(object = tstEnv$lapsi,
                         expected = 5,
                         info = paste0(tstEnv$ylesanne, " Kas asendasid muutuja 'poisse' millegi muuga kui 3?"))
            
          })



# Ülesanne 2.5.1 õige lahendus -----

if(FALSE){
  Fahrenheit=temp*9/5+32
  Fahrenheit
  lisa <- c(-24.9, -16.1)
  nimed=c("Mustvee","Keila")
  names(lisa)=nimed
  lisa
  
}


tstEnv$ylesanne = "Ülesanne 2.05.1"

test_that(tstEnv$ylesanne, 
          {
            # Kontrolliks vajalikud arvutused
            tstEnv$temp_x <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
            names(tstEnv$temp_x) <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")
            tstEnv$lisa_x <- c(-24.9, -16.1)
            names(tstEnv$lisa_x) = c("Mustvee","Keila")
            temp2_x <- c(tstEnv$temp_x,tstEnv$lisa_x)
            
            #1
            expect_true(length(grep("^fahrenheit[=|<-]temp", tolower(tstEnv$tmp_file))) > 0 |
                          length(grep("^farenheit[=|<-]temp", tolower(tstEnv$tmp_file))) > 0,  # Leidub kirjavigu
                        info = paste0(tstEnv$ylesanne, ".1 käsus pole kasutatud muutujat 'temp'"),
                        label = paste0(tstEnv$ylesanne, ".1 käsu kontroll"))
            
            
            expect_true(length(grep("^fahrenheit$", tolower(tstEnv$tmp_file))) > 0 |
                          length(grep("^farenheit$", tolower(tstEnv$tmp_file))) > 0,  # Leidub kirjavigu
                        info = paste0(tstEnv$ylesanne, ".1 käsus pole muutujat Fahrenheit välja prinditud"),
                        label = paste0(tstEnv$ylesanne, ".1 käsu kontroll"))
            
            #2
            expect_true(length(grep("^names\\(lisa\\)", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".2 pole lisatud pealkirjasid"),
                        label = paste0(tstEnv$ylesanne, ".1 käsu kontroll"))
            
            expect_equal(object = tstEnv$lisa,
                         expected = tstEnv$lisa_x,
                         info = paste0(tstEnv$ylesanne, ".2 valesti lisatud uued nimed"))
            
            #3
            expect_equal(object = tstEnv$temp2,
                         expected = tstEnv$temp2_x,
                         info = paste0(tstEnv$ylesanne, ".3 valesti tehtud vektor"))
            
          })


# Ülesanne 2.6.1 õige lahendus -----
if(FALSE){

  # Ülesanne 1: Rakenda funktsiooni exp() ja pane kirja vastus (asenda alakriips sobiva koodiga)
  exp(temp2)
  vastus1 <- "jah"
  
  
  # Ülesanne 2: Rakenda funktsiooni summary() ja pane kirja vastus (asenda alakriips sobiva koodiga)
  summary(temp2)
  vastus2 <- -16.10
  
  
  
  # Ülesanne 3: Rakenda funktsiooni sd() ja pane kirja vastus (asenda alakriips sobiva koodiga)
  sd(temp2)
  vastus3 <- "ei"

}

tstEnv$ylesanne = "Ülesanne 2.06.1"

test_that(tstEnv$ylesanne, 
          {
            #1
            expect_equal(object = tolower(tstEnv$vastus1),
                         expected = 'jah',
                         info = paste0(tstEnv$ylesanne, ".1 muutujale 'vastus1' on antud vale väärtus "))
            
            expect_true(length(grep("^exp\\(temp2\\)$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".1 midagi läks valesti"),
                        label = paste0(tstEnv$ylesanne, ".1 funktsiooni rakendamise kontroll"))
            
            #2
            expect_equal(object = tstEnv$vastus2,
                         expected = -16.1,
                         info = paste0(tstEnv$ylesanne, ".2 muutujale 'vastus2' on antud vale väärtus "))
            
            
            expect_true(length(grep("^summary\\(temp2\\)$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".2 pole 'summary' funktsiooni kasutatud"),
                        label = paste0(tstEnv$ylesanne, ".2 funktsiooni rakendamise kontroll"))
            
            #3
            expect_equal(object = tolower(tstEnv$vastus3),
                         expected = 'ei',
                         info = paste0(tstEnv$ylesanne, ".3 muutujale 'vastus3' on antud vale väärtus "))
            
            
            expect_true(length(grep("^sd\\(temp2\\)$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".3 pole 'sd' funktsiooni kasutatud"),
                        label = paste0(tstEnv$ylesanne, ".3 funktsiooni rakendamise kontroll"))
          })





# Ülesanne 2.7.1 õige lahendus -----
if(FALSE){
  # Ülesanne 1: Vali nõutud elemendid temperatuurivektorist, omista tulemus muutujale vastus1. 
  # Prindi tulemus ekraanile
  vastus1 <- temp[seq(0,length(temp),2)]
  vastus1
  
  
  # Ülesanne 2: Vali välja tingimusele vastavad ilmajaamade nimed, omista tulemus muutujale vastus2. 
  # Prindi tulemus ekraanile
  
  vastus2 <-  jaam[temp <= -17]
  vastus2

}

tstEnv$ylesanne = "Ülesanne 2.07.1"

test_that(tstEnv$ylesanne, 
          { # Algandmete uuesti defineerimine, et tudeng pole neid üle kirjutanud
            tstEnv$temp_x <- c(-6.2, -12.9, -13.0, -15.4, -16.1, -16.9, -17.0, -19.6, -19.9)
            names(tstEnv$temp_x) <- c("Ruhnu", "Kihnu", "Pakri", "Tallinn", "Pärnu", "Kunda", "Kuusiku", "Võru", "Jõgeva")
            
            # Kolmas punkt
            tstEnv$temp2 =c(tstEnv$temp,tstEnv$lisa)
            
            
            #2
            expect_equal(object = tstEnv$vastus1,
                         expected =  tstEnv$temp_x[seq(0,length(tstEnv$temp_x),2)],
                         info = paste0(tstEnv$ylesanne, ".2 muutujale 'vastus1' on antud vale väärtus "))
            
            expect_true(length(grep("temp\\[seq\\(", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".2 pole 'seq' funktsiooni kasutatud"),
                        label = paste0(tstEnv$ylesanne, ".2 funktsiooni rakendamise kontroll"))
            
            expect_true(length(grep("^vastus1$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".2 Muutujat 'vastus1' pole välja prinditud"),
                        label = paste0(tstEnv$ylesanne, ".2 väljatrüki kontroll"))
            
            #3
            expect_equal(object = tstEnv$vastus2,
                         expected =  tstEnv$jaam[tstEnv$temp_x <= -17],
                         info = paste0(tstEnv$ylesanne, ".3 muutujale 'vastus2' on antud vale väärtus "))
            
            expect_true(length(grep("^vastus2$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".3 Muutujat 'vastus2' pole välja prinditud"),
                        label = paste0(tstEnv$ylesanne, ".3 väljatrüki kontroll"))
            
            
          })



# Ülesanne 2.8.1 õige lahendus -----
if(FALSE){

  muutuja1 <- c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)
  muutuja2 <- c(1:3, NA, 0, Inf - Inf)
  muutuja3 <- 1:6
  muutuja1; muutuja2; muutuja3
  # Ülesanne 1: Kontrolli kas vektor muutuja1 on tõeväärtusvektor 
  # (asenda alakriipis sobivalt, et moodustuks õige funktsiooni nimi).
  is.logical(muutuja1)
  
  # Ülesanne 2: Rakenda funktsiooni is.nan() vektorile muutuja2.
  is.nan(muutuja2)
  
  # Ülesanne 3: Asenda vektori muutuja3 esimene element puuduva väärtusega, selleks asenda 
  # järgmises käsus alakriipis sobiva tõeväärtusega. Prindi tulemus ekraanile.
  is.na(muutuja3)[1] <- T
  muutuja3
  
}

tstEnv$ylesanne = "Ülesanne 2.08.1"

test_that(tstEnv$ylesanne, 
          { 
            tstEnv$testi_muutuja3 <- 1:6
            is.na(tstEnv$testi_muutuja3)[1] <- T
            
            #1
            expect_true(length(grep("^is\\.logical\\(muutuja1\\)", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".1 Pole kasutatud õiget funktsiooni"),
                        label = paste0(tstEnv$ylesanne, ".1 Funktsiooni rakendamise kontroll"))
            
            # expect_equal(object = is.logical(muutuja1),
            #              expected =  is.logical(c("TRUE", "true",  "Tru", "FALSE", "F", "false", NA)),
            #              info = paste0(ylesanne, ".3 muutujale 'vastus2' on antud vale väärtus "))
            
            #2
            expect_true(length(grep("^is\\.nan\\(muutuja2\\)", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".2 Pole kasutatud õiget funktsiooni"),
                        label = paste0(tstEnv$ylesanne, ".2 Funktsiooni rakendamise kontroll"))
            
            #3
            expect_equal(object = tstEnv$muutuja3,
                         expected =  tstEnv$testi_muutuja3,
                         info = paste0(tstEnv$ylesanne, ".3 Puuduv väärtus on valesti määratud"))
            
            expect_true(length(grep("^is\\.na\\(muutuja3\\)\\[1\\][=|<-]NA", tstEnv$tmp_file)) == 0, 
                        info = paste0(tstEnv$ylesanne, ".3 Valesti määratud väärtus, kasutatud 'NA'-d"),
                        label = paste0(tstEnv$ylesanne, ".3 Teada vale vastuse kontroll"))
            
            expect_true(length(grep("^muutuja3$", tstEnv$tmp_file)) > 0, 
                        info = paste0(tstEnv$ylesanne, ".3 Muutujat 'muutuja3' pole välja prinditud"),
                        label = paste0(tstEnv$ylesanne, ".3 väljatrüki kontroll"))
          })



# Ülesanne 2.9.1 õige lahendus -----
if(FALSE){
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
  
  #Ülesanne 1: Väärtusta tõeväärtusvektor x4. Prindi x4 ekraanile
  x4 <- x > 30
  x4
  
  
  #Ülesanne 2: Moodusta tõeväärtusvektor vektor x5. Prindi  ekraanile
  x5 <- x<40
  x5
  
  
  #Ülesanne 3: Moodusta tõeväärtusvektor x6 kasutades vektoreid x4 ja x5 
  # ning sobivat loogilist tehet. Prindi tulemus ekraanile
  x6 <- x4 & x5
  x6

}

tstEnv$ylesanne = "Ülesanne 2.09.1"

test_that(tstEnv$ylesanne, 
          {
            tstEnv$x_test <- c(34, 23, 45, 67, 10, 21, 37)
            tstEnv$x_test4 = x_test > 30
            tstEnv$x_test5 = x_test < 40
            
            #1
            expect_equal(object = tstEnv$x4, 
                        expected = tstEnv$x_test4,
                        label = paste0(ylesanne, ".1 Valesti koostatud tõeväärtusvektor"))
            
            #2
            expect_equal(object = tstEnv$x5,
                         expected = tstEnv$x_test5,
                         info = paste0(tstEnv$ylesanne, ".2 Valesti koostatud tõeväärtusvektor"))
            
            #3
            expect_equal(object = tstEnv$x6,
                         expected = tstEnv$x_test4 & tstEnv$x_test5,
                         info = paste0(tstEnv$ylesanne, ".3 Valesti koostatud tõeväärtusvektor"))
            
          })



# Ülesanne 2.10.1 õige lahendus -----
if(FALSE){

  # Näide: vaatame tähestiku algust, moodustame kolme moodi tõevektori, mille väärtus on TRUE, 
  # kui täht on a või b ning on FALSE vastasel korral
  abc <- letters[1:3]
  abc
  abc == "a" | abc == "b" # täht on 'a' või täht on 'b'
  abc != "c"  # täht ei ole 'c'
  !(abc == "c")  # eitame väidet, et täht on 'c'
  
  
  # Jooksuta järgnev kood
  kordused <- c(45, 20, 68, 9)
  y <- sample(rep(c("tere", "hei", "tsau", "hommikust"), times = kordused), size = sum(kordused))
  y
  # Ülesandes on uurimise all tekstiväärtustega vektor y, vaata selle väärtuste sagedustabelit
  table(y)
  
  #1.Leia tõevektor, mis näitaks millised vektori y väärtused vastavad sõnele “tere” või “tsau”.
  viimane= y =="tere" | y=="tsau"
  viimane
  table(viimane)


}

tstEnv$ylesanne = "Ülesanne 2.10.1"

test_that(tstEnv$ylesanne, 
          {
            #1
            expect_equal(object = tstEnv$viimane,
                         expected = tstEnv$y =="tere" | tstEnv$y=="tsau",
                         info = paste0(tstEnv$ylesanne, ".1 Valesti koostatud vektor"))
            
          })


