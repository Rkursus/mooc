library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 2 kontroll")

# Ülesanne 1.1.1 õige lahendus -----
if(FALSE){
  # Ülesanne 1: Impordi andmed, moodustades objekti andmed2. Prindi saadud objekt ekraanile
  andmed2 <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/tulemused.txt", header = T, sep = "\\", dec = ",")
  andmed2
  
  
  # Ülesanne 2: Vaata andmestiku tunnuste ülevaadet
  summary(andmed2)
  
}

ylesanne = "Ülesanne 1.1.1"

test_that(ylesanne, 
  {
    expect_equal(object = colnames(andmed2),
                 expected = c("nimi", "tulemus"),
                 info = paste0(ylesanne, " Tabeli veerud ei ühti"))
    
    #expect_true(length(grep("read\\.table\\(.+(header.+sep.+dec|sep.+header.+dec|dec.+sep.+header|sep.+dec.+header).+\\)", tmp_file)) > 0, 
    #          info = paste0(ylesanne, " käsust on midagi puudu"))
    
    expect_true(length(grep("^andmed2$", tmp_file)) > 0, 
                info = paste0(ylesanne, " pole andmestikku välja prinditud"))
    
    expect_true(length(grep("^summary\\(andmed2\\)$", tmp_file)) > 0, 
                info = paste0(ylesanne, " pole andmestikku kokkuvõtet välja prinditud"))
    
  })

# Ülesanne 1.2.1 õige lahendus -----

# Valikvastustega küsimus
# Õige variant oli kolmas


# Ülesanne 1.3.1 õige lahendus -----
if(FALSE){
  # Ülesanne 1: Omista muutujale selle argumendi nimi, mis määrab imporditava ridade arvu
  argumendinimi <- "nrows"
  
  
  # Ülesanne 2: kontrolli faili olemasolu töökaustas
  list.files() 
  
  
  # Ülesanne 3: täienda antud koodi
  andmed4 <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
  
  
  # Ülesanne 4: prindi ekraanile nõutud alamosa andmestikust
  valik <- andmed4[1:10, -(4:5)]
  valik
  
}

ylesanne = "Ülesanne 1.3.1"

test_that(ylesanne, 
          { 
            #1
            expect_true(grep("^nrows$|^nrow\\(\\)$", argumendinimi) > 0,
                        info = paste0(ylesanne, ".1 Vale vastus"),
                        label = paste0(ylesanne, ".1 vastuse kontroll"))
            
            #2
            expect_true(length(grep("list\\.files\\(\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".2 käsust on midagi puudu"),
                        label = paste0(ylesanne, ".2 käsu kontroll"))
            
            #3
            expect_true(length(grep("read\\.csv2\\(.?(A\\.csv.+nrows).+\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, " pole kasutatud nõutud funktsiooni"),
                        label = paste0(ylesanne, ".3 faili laadimise kontroll"))
            
            expect_equal(object = andmed4,
                         expected = read.csv2("https://github.com/Rkursus/2020/raw/master/data/A.csv", nrows = 45),
                         info = paste0(ylesanne, ".3 Vale vastus, andmestik ei kattu"))
            
            #4
            expect_equal(object = valik,
                         expected = andmed4[1:10, -(4:5)],
                         info = paste0(ylesanne, ".4 Vale vastus, andmestik ei kattu"))
            
            expect_true(length(grep("^valik$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".4 pole andmestikku välja prinditud"),
                        label = paste0(ylesanne, ".4 andmestiku väljarüki kontroll"))
            
          })



# Ülesanne 1.4.1 õige lahendus -----
if(FALSE){
# Ülesanne 1: Pane kirja sobivad funktsiooninimed, ning viimases käsus sobivad indeksid
dim(andmed5a)
str(andmed5a)
tail(andmed5a[ , 1:5 ])


# Ülesanne 2: tee antud koodi vajalik täiendus, prindi tulemus ekraanile
andmed5 <- read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160, stringsAsFactors = FALSE)


# Ülesanne 3: vali andmestikust nõutud alamosa
valik <- andmed5[, substr(names(andmed5), 1, 5) %in% c("taust", "hinna")]
valik
}


ylesanne = "Ülesanne 1.4.1"

test_that(ylesanne, 
          {
            #1
            expect_true(length(grep("dim\\(andmed5a\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 käsu kontroll"))
            
            expect_true(length(grep("str\\(andmed5a\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 käsu kontroll"))
            
            expect_true(length(grep("^tail\\(andmed5a\\[.?(1\\:5)\\]\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 käsu kontroll"))
            
            #2
            expect_equal(object = andmed5,
                         expected = read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", 
                                              nrows = 160, 
                                              stringsAsFactors = FALSE),
                         info = paste0(ylesanne, ".2 Valesti sisse loetud andmestik"))
            # Ülesande teksti sooviti, et tekstilised väärtused oleksid kindlasti tekstid, mitte faktorid, seega peaks argument stringsAsFactors = FALSE.
            
            
            #3
            expect_equal(object = valik,
                         expected = andmed5[, substr(names(andmed5), 1, 5) %in% c("taust", "hinna")],
                         info = paste0(ylesanne, ".3 Valesti tehtud valik andmestik"))
            
          })


# Ülesanne 2.1.1 õige lahendus -----
if(FALSE){

# Ülesanne 1: prindi andmestik ekraanile
pojad

# Ülesanne 2: moodusta tõeväärtusvektor
filter <- pojad$l1 > mean(pojad$l1)
filter


# Ülesanne 3: vali tingimusele vastavad read andmestikust, küsi andmestiku dimensioone
pojad1 <- pojad[filter, ]
dim(pojad1)

}

ylesanne = "Ülesanne 2.1.1"

test_that(ylesanne, 
          {
            #1
            expect_equal(object = pojad[, c("l1", "b1", "l2", "b2")],
                         expected = read.table("https://github.com/Rkursus/2020/raw/master/data/pojad.txt", header = T),
                         info = paste0(ylesanne, ".1 Valesti sisse loetud andmestik"))
            
            expect_true(length(grep("^pojad$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 pole andmestikku välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku väljarüki kontroll"))
            #2
            expect_equal(object = filter,
                         expected = pojad$l1 > mean(pojad$l1),
                         info = paste0(ylesanne, ".2 Valesti defineeritud filter"))
            
            expect_true(length(grep("^filter$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".2 pole 'filtrit' välja prinditud"),
                        label = paste0(ylesanne, ".2 andmestiku väljarüki kontroll"))
            
            #3
            expect_equal(object = pojad1,
                         expected = pojad[filter, ],
                         info = paste0(ylesanne, ".3 Valesti filtreeritud andmed"))
            
            
            expect_true(length(grep("^dim\\(pojad1\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".3 andmestikku 'pojad1' dimensioone pole välja prinditud"),
                        label = paste0(ylesanne, ".3 andmestiku dimensiooin väljarüki kontroll"))
            
          })





# Ülesanne 2.2.1 õige lahendus -----
if(FALSE){

# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga
summary(kapsad)

# Ülesanne 2: moodusta tõeväärtusvektorid
filter1 <- kapsad$Cult == "c52"
filter2 <- kapsad$Date == "d21"


# Ülesanne 3: vali tingimusele vastavad read andmestikust 
kapsad1 <- kapsad[filter1 & filter2, ]
 

# Ülesanne 4:  vali tingimusele vastavad read andmestikust 
kapsad2 <- kapsad[filter1 | filter2, ]


# Ülesanne 5: pane kirja suurema objektide arvuga andmetabeli nimi
kumbsuurem <- "kapsad2"


}

ylesanne = "Ülesanne 2.2.1"

test_that(ylesanne, 
          {
            #1
            expect_equal(object = kapsad,
                         expected = read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T),
                         info = paste0(ylesanne, ".1 Valesti sisse loetud andmestik"))
            
            expect_true(length(grep("^summary\\(kapsad\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 pole andmestikku kokkuvõtet välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku kokkuvõtte väljarüki kontroll"))
            
            #2
            expect_equal(object = filter1,
                         expected = kapsad$Cult == "c52",
                         info = paste0(ylesanne, ".2 Valesti defineeritud filter1"))
            
            expect_equal(object = filter2,
                         expected = kapsad$Date == "d21",
                         info = paste0(ylesanne, ".2 Valesti defineeritud filter2"))
            
            #3
            expect_equal(object = kapsad1,
                         expected = kapsad[filter1 & filter2, ],
                         info = paste0(ylesanne, ".3 Valesti filtreeritud andmed"))
            
            #4
            expect_equal(object = kapsad2,
                         expected = kapsad[filter1 | filter2, ],
                         info = paste0(ylesanne, ".4 Valesti filtreeritud andmed"))
            
            #5
            
          })



# Ülesanne 2.3.1 õige lahendus -----
if(FALSE){

# Vaata meeldetuletuseks andmestikku:
summary(pojad)

# Ülesanne 1: uue tunnuse lisamine, vahe
pojad$pikkus_vahe <- pojad$l1 - pojad$l2


# Ülesanne 2: uue tunnuse lisamine, suhe
pojad$laius_suhe <- pojad$b1 / pojad$b2


# Ülesanne 3: andmestiku veergude ümberjärjestamine
uus <- pojad[, c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")]

}

ylesanne = "Ülesanne 2.3.1"

test_that(ylesanne, 
          {
            #1
            expect_true("pikkus_vahe" %in% colnames(pojad),
                        info = paste0(ylesanne, ".1 andmestikust puudub tunnus 'pikkus_vahe'"))
            
            expect_equal(object = pojad$pikkus_vahe,
                         expected = pojad$l1 - pojad$l2,
                         info = paste0(ylesanne, ".1 Valesti defineeritud tunnus 'pikkus_vahe'"))
            
            #2
            expect_true("laius_suhe" %in% colnames(pojad) | "laus_suhe" %in% colnames(pojad),
                        info = paste0(ylesanne, ".2 andmestikust puudub tunnus 'laius_suhe'"))
            
            expect_equal(object = pojad$laius_suhe,
                         expected = pojad$b1 / pojad$b2,
                         info = paste0(ylesanne, ".2 Valesti defineeritud tunnus 'laius_suhe'"))
            
            #3
            expect_equal(object = uus,
                         expected = pojad[ , c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")],
                         info = paste0(ylesanne, ".3 Valesti filtreeritud andmed"))
            
          })



# Ülesanne 2.4.1 õige lahendus -----
if(FALSE){
# Ülesanne 1: prindi andmestiku kirjeldus nõutud käsuga
summary(dieet)


# Ülesanne 2: Tee grammides kaalule teisendus kilodeks
dieet[which.max(dieet$kaal1), "kaal1"] <- dieet[which.max(dieet$kaal1), "kaal1"] /1000


# Ülesanne 3: Asenda vigane väärtus tühikuga
is.na(dieet[which.max(dieet$kaal2), "kaal2"]) <- TRUE

}

ylesanne = "Ülesanne 2.4.1"

test_that(ylesanne, 
          {
            #1
            expect_true(length(grep("^summary\\(dieet\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1 pole andmestikku kokkuvõtet välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku kokkuvõtte väljarüki kontroll"))
            
            #2
            expect_equal(object = sum(dieet$kaal1),
                         expected = 28978,
                         info = paste0(ylesanne, ".2 Vale elemendi väärtus muudetud"))
            
            #3
            expect_true(object = sum(is.na(dieet$kaal2)) == 1,
                         info = paste0(ylesanne, ".3 Puuduv väärtus on valesti lisatud"))
            
          })



# Ülesanne 2.5.1 õige lahendus -----
if(FALSE){

# Ülesanne 1: leia isikud, kes on andmestikus B, kuid mitte A-s
olemasBmitteA <- setdiff(B$id, A$id)


# Ülesanne 2: leia isikud, kes on mõlemas andmestikus
AjaB <- sort(intersect(B$id, A$id), decreasing = TRUE)


# Ülesanne 3: Liida anmdestikud, tulemuses olgu kõik isikud mõlemast andmestikust.
uuring1 <- merge(A, B, by = "id", all = TRUE)


# Ülesanne 4: leia nõutud keskväärtus ...
kesk <- mean(uuring1$test101, na.rm = T)
# ... ja standardhälve
stand <- sd(uuring1$test101, na.rm = T)

# Ülesanne 5:  Liida andmestikud, tulemuses olgu kõik isikud kel on mõlemas andmestikus info olemas.
uuring2 <-  merge(A, B, by = "id", all = FALSE)

# Ülesanne 6: faktortunnuse loomine
uuring2$sugu2 <- factor(uuring2$sugu, labels = c("Naine", "Mees"))

# Ülesanne 7: Sagedustabel
tabel1 <- table(uuring2$grupp, uuring2$sugu2)
tabel1

# Ülesanne 8: Jaotustabel
tabel2 <-  prop.table(tabel1, 1)
tabel2

# Ülesanne 9: Naiste arv grupis c
c.naisi <- 1


}

ylesanne = "Ülesanne 2.5.1"

test_that(ylesanne, 
          {
            A_tmp <- read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
            B_tmp <- read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
            B_tmp <- B_tmp[, c("id", "grupp", sort(names(B_tmp)[-(1:2)]))]
            
            #1
            expect_equivalent(object = olemasBmitteA,
                         expected = setdiff(B_tmp$id, A_tmp$id),
                         info = paste0(ylesanne, ".1 Vale elementide nimekiri"))
            
            #2
            expect_equal(object = AjaB,
                         expected = sort(intersect(B_tmp$id, A_tmp$id), decreasing = TRUE),
                         info = paste0(ylesanne, ".2 Vale elemendi väärtus muudetud"))
            
            #3
            expect_equivalent(object = uuring1,
                         expected = merge(A_tmp, B_tmp, by = "id", all = TRUE),
                         info = paste0(ylesanne, ".3 Valesti koostatnud tabel"))
            
            #4
            expect_equal(object = kesk,
                         expected = mean(uuring1$test101, na.rm = T),
                         info = paste0(ylesanne, ".4 Valesti defineeritud 'kesk'"))
            
            expect_equal(object = stand,
                         expected = sd(uuring1$test101, na.rm = T),
                         info = paste0(ylesanne, ".4 Valesti defineeritud 'stand'"))
            
            #5
            expect_equivalent(object = uuring2[,1:86],
                         expected =  merge(A, B, by = "id", all.x = FALSE),
                         info = paste0(ylesanne, ".5 Vale elemendi väärtus muudetud"))
            
            #6
            expect_true(object = "sugu2" %in% colnames(uuring2),
                        info = paste0(ylesanne, ".6 Puudub tunnus 'sugu2'"))
            
            expect_equivalent(object = uuring2$sugu2,
                         expected =  factor(uuring2$sugu, labels = c("Naine", "Mees")),
                         info = paste0(ylesanne, ".6 Valesti lisatud tunnus 'sugu2'"))
            
            #7
            expect_equal(object = tabel1,
                         expected = table(uuring2$grupp, uuring2$sugu2),
                         info = paste0(ylesanne, ".7 Vale sagedustabel"))
            
            #8
            expect_equal(object = tabel2,
                         expected = prop.table(tabel1, 1),
                         info = paste0(ylesanne, ".8 Vale proportsioonide tabel"))
            
            #9
            expect_true(object = c.naisi == 1,
                        info = paste0(ylesanne, ".9 Vale vastus"))
          })


