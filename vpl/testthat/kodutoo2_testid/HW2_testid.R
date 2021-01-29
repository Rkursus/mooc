library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

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
yl = 1

test_that(ylesanne, 
  {
    eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
    
    veerud_expected = c("nimi", "tulemus")
    expect_equal(object = colnames(andmed2),
                 expected = veerud_expected,
                 info = paste0(ylesanne, ".1: tabeli veerud ei ühti"),
                 label = paste0(ylesanne, ".1 andmestiku kontroll"))
    
    expect_true(length(grep("read\\.table\\(.+(header.+sep.+dec|sep.+header.+dec|dec.+sep.+header|sep.+dec.+header|header.+dec.+sep|dec.+header.+sep).+\\)", tmp_file)) > 0, 
              info = paste0(ylesanne, ".1: käsust on midagi puudu"),
              label = paste0(ylesanne, ".1 käsu kontroll"))
    
    expect_true(length(grep("^andmed2$|^print\\(andmed2\\)$", tmp_file)) > 0, 
                info = paste0(ylesanne, ".1: pole andmestikku välja prinditud"),
                label = paste0(ylesanne, ".1 andmestiku väljatrüki kontroll"))
    
    expect_true(length(grep("^summary\\(andmed2\\)$|^print\\(summary\\(andmed2\\)\\)$", tmp_file)) > 0, 
                info = paste0(ylesanne, ".2: pole andmestiku kokkuvõtet välja prinditud"),
                label = paste0(ylesanne, ".2 kokkuvõtte väljatrüki kontroll"))
    
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
yl = 3

test_that(ylesanne, 
          { 
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            expect_true(grep("^nrows$|^nrow\\(\\)$", argumendinimi) > 0,
                        info = paste0(ylesanne, ".1: vale vastus"),
                        label = paste0(ylesanne, ".1 vastuse kontroll"))
            
            #2
            expect_true(length(grep("list\\.files\\(\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".2: käsust on midagi puudu"),
                        label = paste0(ylesanne, ".2 käsu kontroll"))
            
            #3
            expect_true(length(grep("read\\.csv2\\(.?(A\\.csv.+nrows).+\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".3: pole kasutatud nõutud funktsiooni"),
                        label = paste0(ylesanne, ".3 faili laadimise kontroll"))
            
            andmed_test = read.csv2("https://github.com/Rkursus/2020/raw/master/data/A.csv", nrows = 45)
            expect_equal(object = andmed4,
                         expected = andmed_test,
                         info = paste0(ylesanne, ".3: vale vastus, andmestik ei kattu"),
                         label = paste0(ylesanne, ".3 andmestiku kontroll"))
            
            #4
            valik_expected = andmed4[1:10, -(4:5)]
            expect_equal(object = valik,
                         expected = valik_expected,
                         info = paste0(ylesanne, ".4: vale vastus, andmestik ei kattu"),
                         label = paste0(ylesanne, ".4 valiku kontroll"))
            
            expect_true(length(grep("^valik$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".4: pole andmestikku välja prinditud"),
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
yl=4

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            expect_true(length(grep("dim\\(andmed5a\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: dimensiooni käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 dimensiooni käsu kontroll"))
            
            expect_true(length(grep("str\\(andmed5a\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: tunnuste käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 tunnuste käsu kontroll"))
            
            expect_true(length(grep("^tail\\(andmed5a\\[.?(1\\:5)\\]\\)", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: 'tail' käsust on midagi puudu"),
                        label = paste0(ylesanne, ".1 'tail' käsu kontroll"))
            
            #2
            andmed5_expected = read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", 
                                         nrows = 160, 
                                         stringsAsFactors = FALSE)
            expect_equal(object = andmed5,
                         expected = andmed5_expected,
                         info = paste0(ylesanne, ".2: valesti sisse loetud andmestik"),
                         label = paste0(ylesanne,".3 andmestiku kontroll"))
            # Ülesande teksti sooviti, et tekstilised väärtused oleksid kindlasti tekstid, mitte faktorid, seega peaks argument stringsAsFactors = FALSE.
            
            
            #3
            valik_expected = andmed5[, substr(names(andmed5), 1, 5) %in% c("taust", "hinna")]
            expect_equal(object = valik,
                         expected = valik_expected,
                         info = paste0(ylesanne, ".3: valesti tehtud valik-andmestik"),
                         label = paste0(ylesanne,".3 valiku kontroll"))
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
yl=5

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            pojad_expected = read.table("https://github.com/Rkursus/2020/raw/master/data/pojad.txt", header = T)
            expect_equal(object = pojad[, c("l1", "b1", "l2", "b2")],
                         expected = pojad_expected,
                         info = paste0(ylesanne, ".1: valesti sisse loetud andmestik"),
                         label = paste0(ylesanne,".1 andmestiku sisselugemise kontroll"))
            
            expect_true(length(grep("^pojad$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: pole andmestikku välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku väljarüki kontroll"))
            #2
            filter_expected = pojad$l1 > mean(pojad$l1)
            expect_equal(object = filter,
                         expected = filter_expected,
                         info = paste0(ylesanne, ".2: valesti defineeritud filter"),
                         label = paste0(ylesanne,".2 filtri kontroll"))
            
            expect_true(length(grep("^filter$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".2: pole 'filtrit' välja prinditud"),
                        label = paste0(ylesanne, ".2 filtri väljarüki kontroll"))
            
            #3
            pojad1_expected = pojad[filter, ]
            expect_equal(object = pojad1,
                         expected = pojad1_expected,
                         info = paste0(ylesanne, ".3: valesti filtreeritud andmed"),
                         label = paste0(ylesanne,".3 filtreeritud andmete kontroll "))
            
            
            expect_true(length(grep("^dim\\(pojad1\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".3: andmestiku 'pojad1' dimensioone pole välja prinditud"),
                        label = paste0(ylesanne, ".3 andmestiku dimensiooni väljarüki kontroll"))
            
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
yl = 6

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            kapsad_expected = read.table("https://github.com/Rkursus/sygis2019/raw/master/data/cabbages.txt", header = T)
            expect_equal(object = kapsad,
                         expected = kapsad_expected,
                         info = paste0(ylesanne, ".1: valesti sisse loetud andmestik"),
                         label = paste0(ylesanne, ".1 andmestiku sisselugemise kontroll"))
            
            expect_true(length(grep("^summary\\(kapsad\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: pole andmestiku kokkuvõtet välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku kokkuvõtte väljarüki kontroll"))
            
            #2
            filter_expected = kapsad$Cult == "c52"
            expect_equal(object = filter1,
                         expected = filter_expected,
                         info = paste0(ylesanne, ".2: valesti defineeritud filter1"),
                         label = paste0(ylesanne,".2 filter1 kontroll"))
            
            filter_expected = kapsad$Date == "d21"
            expect_equal(object = filter2,
                         expected = filter_expected,
                         info = paste0(ylesanne, ".2: valesti defineeritud filter2"),
                         label = paste0(ylesanne,".2 filter2 kontroll"))
            
            #3
            kapsad1_expected = kapsad[filter1 & filter2, ]
            expect_equal(object = kapsad1,
                         expected = kapsad1_expected,
                         info = paste0(ylesanne, ".3: valesti filtreeritud andmed"),
                         label = paste0(ylesanne,".3 filtreeritud andmete kontroll"))
            
            #4
            kapsad2_expected = kapsad[filter1 | filter2, ]
            expect_equal(object = kapsad2,
                         expected = kapsad2_expected,
                         info = paste0(ylesanne, ".4: valesti filtreeritud andmed"),
                         label = paste0(ylesanne, ".4 filtreeritud andmete kontroll"))
            
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
yl = 7

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            expect_true("pikkus_vahe" %in% colnames(pojad),
                        info = paste0(ylesanne, ".1: andmestikust puudub tunnus 'pikkus_vahe'"),
                        label = paste0(ylesanne,".1 tunnuse olemasolu kontroll"))
            
            pikkus_vahe_expected = pojad$l1 - pojad$l2
            expect_equal(object = pojad$pikkus_vahe,
                         expected = pikkus_vahe_expected,
                         info = paste0(ylesanne, ".1: valesti defineeritud tunnus 'pikkus_vahe'"),
                         label = paste0(ylesanne, ".1 'pikkus_vahe' kontroll"))
            
            #2
            expect_true("laius_suhe" %in% colnames(pojad) | "laus_suhe" %in% colnames(pojad),
                        info = paste0(ylesanne, ".2: andmestikust puudub tunnus 'laius_suhe'"),
                        label = paste0(ylesanne,".2 tunnuse olemasolu kontroll"))
            
            laius_suhe_expected = pojad$b1 / pojad$b2
            expect_equal(object = pojad$laius_suhe,
                         expected = laius_suhe_expected,
                         info = paste0(ylesanne, ".2: valesti defineeritud tunnus 'laius_suhe'"),
                         label = paste0(ylesanne,".2 'laius_suhe' kontroll"))
            
            #3
            uus_expected = pojad[ , c("l1", "l2", "pikkus_vahe", "b1", "b2", "laius_suhe")]
            expect_equal(object = uus,
                         expected = uus_expected,
                         info = paste0(ylesanne, ".3: valesti ümberjärjestatud andmed"),
                         label = paste0(ylesanne, ".3 ümberjärjestuse kontroll"))
            
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
yl = 8

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            #1
            expect_true(length(grep("^summary\\(dieet\\)$", tmp_file)) > 0, 
                        info = paste0(ylesanne, ".1: pole andmestiku kokkuvõtet välja prinditud"),
                        label = paste0(ylesanne, ".1 andmestiku kokkuvõtte väljatrüki kontroll"))
            
            #2
            expect_true(object = sum(dieet$kaal1)==28978,
                         info = paste0(ylesanne, ".2: oodatud teisendus tegemata"),
                         label = paste0(ylesanne,".2 teisenduse kontroll"))
            
            #3
            expect_true(object = (sum(is.na(dieet$kaal2)) == 1),# | (sum(dieet$kaal2==""|dieet$kaal2==" ")==1)? <- ülesandes palutud tühikuga asendada
                         info = paste0(ylesanne, ".3: puuduv väärtus on valesti lisatud"),
                        label = paste0(ylesanne, ".3 asenduse kontroll"))
            
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
yl = 9

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            A_tmp <- read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
            B_tmp <- read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
            B_tmp <- B_tmp[, c("id", "grupp", sort(names(B_tmp)[-(1:2)]))]
            
            #1
            olemasBmitteA_expected = setdiff(B_tmp$id, A_tmp$id)
            expect_equivalent(object = olemasBmitteA,
                         expected = olemasBmitteA_expected,
                         info = paste0(ylesanne, ".1: vale elementide nimekiri"),
                         label = paste0(ylesanne, ".1 elementide nimekirja kontroll"))
            
            #2
            AjaB_expected = sort(intersect(B_tmp$id, A_tmp$id), decreasing = TRUE)
            expect_equal(object = AjaB,
                         expected = AjaB_expected,
                         info = paste0(ylesanne, ".2: sorteeritud vektor ei vasta oodatule"),
                         label = paste0(ylesanne, ".2 sorteeritud vektori kontroll"))
            
            #3
            uuring1_expected = merge(A_tmp, B_tmp, by = "id", all = TRUE)
            expect_equivalent(object = uuring1,
                         expected = uuring1_expected,
                         info = paste0(ylesanne, ".3: andmestik on valesti ühendatud"),
                         label = paste0(ylesanne, ".3 ühendatud andmestiku kontroll"))
            
            #4
            kesk_expected = mean(uuring1$test101, na.rm = T)
            expect_equal(object = kesk,
                         expected = kesk_expected,
                         info = paste0(ylesanne, ".4: valesti defineeritud 'kesk'"),
                         label = paste0(ylesanne, ".4 keskväärtuse kontroll"))
            
            stand_expected = sd(uuring1$test101, na.rm = T)
            expect_equal(object = stand,
                         expected = stand_expected,
                         info = paste0(ylesanne, ".4: valesti defineeritud 'stand'"),
                         label = paste0(ylesanne, ".4 standardhälbe kontroll"))
            
            #5
            uuring2_expected = merge(A, B, by = "id", all.x = FALSE)
            expect_equivalent(object = uuring2[,1:86],
                         expected =  uuring2_expected,
                         info = paste0(ylesanne, ".5: andmestik on valesti ühendatud"),
                         label = paste0(ylesanne,".5 ühendatud andmestiku kontroll"))
            
            #6
            expect_true(object = "sugu2" %in% colnames(uuring2),
                        info = paste0(ylesanne, ".6: puudub tunnus 'sugu2'"),
                        label = paste0(ylesanne, ".6 uue tunnuse olemasolu kontroll"))
            
            sugu2_expected = factor(uuring2$sugu, labels = c("Naine", "Mees"))
            expect_equivalent(object = uuring2$sugu2,
                         expected =  sugu2_expected,
                         info = paste0(ylesanne, ".6: valesti lisatud tunnus 'sugu2'"),
                         label = paste0(ylesanne,".6 uue tunnuse kontroll"))
            
            #7
            tabel1_expected = table(uuring2$grupp, uuring2$sugu2)
            expect_equal(object = tabel1,
                         expected = tabel1_expected,
                         info = paste0(ylesanne, ".7: vale sagedustabel"),
                         label = paste0(ylesanne,".7 sagedustabeli kontroll"))
            
            expect_true(length(grep("^tabel1$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".7: tabelit pole välja prinditud"),
                        label = paste0(ylesanne, ".7 tabeli väljatrüki kontroll"))
            
            #8
            tabel2_expected = prop.table(tabel1, 1)
            expect_equal(object = tabel2,
                         expected = tabel2_expected,
                         info = paste0(ylesanne, ".8: vale proportsioonide tabel"),
                         label = paste0(ylesanne, ".8 jaotustabeli kontroll"))
            
            expect_true(length(grep("^tabel2$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".8: tabelit pole välja prinditud"),
                        label = paste0(ylesanne, ".8 tabeli väljatrüki kontroll"))
            
            #9
            expect_true(object = c.naisi == 1,
                        info = paste0(ylesanne, ".9: vale vastus"),
                        label = paste0(ylesanne, ".9 naiste arvu kontroll grupis c"))
          })
