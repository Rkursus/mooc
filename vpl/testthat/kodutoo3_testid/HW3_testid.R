library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 3 kontroll")

# Ülesanne 1.1.1 õige lahendus -----
if(FALSE){
  #1
  summary(iris)
  keskmised = by(iris$Petal.Length, iris$Species, mean)
  
  #2
  #setosa
  
  #3
  iris$sordinimi = factor(iris$Species, levels= c("versicolor","setosa","virginica"))
  
  #4
  maksimumid = tapply(iris$Petal.Length, iris$sordinimi,max)
  maksimumid
}

ylesanne = "Ülesanne 1.1.1"
yl = 1

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            keskmised_test = by(iris$Petal.Length, iris$Species, mean)
            expect_equal(object = keskmised,
                         expected = keskmised_test,
                         info = paste0(ylesanne, ".1: tulemus ei vasta oodatule"))
            
            expect_true(length(grep("mean\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: by() funktsiooni kolmas argument peaks olema 'mean'"),
                        label = paste0(ylesanne, ".1 funktsiooni kontroll"))
            
            expect_equal(names(keskmised_test),names(keskmised), 
                        info = paste0(ylesanne, ".1: vaata üle by() funktsiooni teine argument ehk grupitunnus"))
            
            #3
            expect_true("sordinimi"%in%names(iris),
                        info = paste0(ylesanne, ".3: puudub tunnus nimega 'sordinimi'"),
                        label = paste0(ylesanne, ".3 tunnuse olemasolu kontroll"))
            
            levels_test = c("versicolor","setosa","virginica")
            expect_equal(object = levels(iris$sordinimi),
                         expected = levels_test,
                         info = paste0(ylesanne, ".3: faktori väärtuste järjekord ei vasta oodatule"))
            
            #4
            maksimumid_test = tapply(iris$Petal.Length, iris$sordinimi,max)
            expect_equal(object = maksimumid,
                         expected = maksimumid_test,
                         info = paste0(ylesanne, ".4: tulemus ei vasta oodatule"))
            
            expect_true(length(grep("max\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".4: tapply() funktsiooni kolmas argument peaks olema 'max'"),
                        label = paste0(ylesanne, ".4 funktsiooni kontroll"))
            
            expect_true(length(grep("tapply.+(iris\\$sordinimi|iris[,6]).+\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".4: pole kasutatud eelmises punktis tekitatud tunnust"),
                        label = paste0(ylesanne, ".4 teise argumendi kontroll"))
          })

# Ülesanne 1.2.1 õige lahendus -----
if(FALSE){
  #1
  intervallid = cut(iris$Petal.Length,breaks = seq(1,7,0.5),right=FALSE)
  
  #2
  is.factor(intervallid)
  
  #3
  sagedustabel = table(intervallid)
  sagedustabel
  
  #4
  tyhjad = 2
}

ylesanne = "Ülesanne 1.2.1"
yl = 2

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            intervallid_test = cut(iris$Petal.Length,breaks = seq(1,7,0.5),right=FALSE)
            expect_equal(object = intervallid,
                         expected = intervallid_test,
                         info = paste0(ylesanne, ".1: tulemus ei vasta oodatule"))
            
            expect_true(length(grep("cut\\(.+,.+,.+\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: funktsioonil cut() peab olema kolm argumenti"),
                        label = paste0(ylesanne, ".1 funktsiooni argumentide kontroll"))
            
            expect_true(length(grep("breaks=", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: lõikepunktide määramiseks pole kasutatud õiget argumenti"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            intervalle_kokku = length(levels(intervallid))==length(levels(intervallid_test))
            esimesed = substr(levels(intervallid)[1],2,6)==substr(levels(intervallid_test)[1],2,6)
            viimased = substr(levels(intervallid)[12],2,6)==substr(levels(intervallid_test)[12],2,6)
            expect_true(intervalle_kokku&esimesed&viimased, 
                        info = paste0(ylesanne, ".1: lõikepunktide väärtused ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("right=(F|FALSE)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: argument 'right' pole korrektselt määratud"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            #2
            expect_true(length(grep("is.factor\\(", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: kasuta funktsiooni 'is.factor'"),
                        label = paste0(ylesanne, ".2 funktsiooni kontroll"))
            
            #3
            sagedus_test = table(intervallid)
            expect_equal(object = sagedustabel,
                         expected = sagedus_test,
                         info = paste0(ylesanne, ".3: sagedustabel ei vasta oodatule"))
            
            expect_true(length(grep("table\\(", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".3: sagedustabeli koostamiseks pole kasutatud õiget funktsiooni"),
                        label = paste0(ylesanne, ".3 funktsiooni kontroll"))
            
            #4
            expect_true(tyhjad==2,
                        info = paste0(ylesanne, ".4: muutuja 'tyhjad' väärtus ei ole korrektne"),
                        label = paste0(ylesanne, ".4 muutuja 'tyhjad' kontroll"))
            
          })




# Ülesanne 2.1.1 õige lahendus -----
if(FALSE){
  x <- c(2:1, 2:1, 2:1, 4)
  y <- c(7, 1, 5, 2, 6, 3, 4)
  xy <- data.frame(x, y)
  x; y; xy
  
  #1
  xy1 = xy[order(x,y),]
  xy1
  
  #2
  xy2 = xy[order(x,-y),]
  xy2
  
  #3
  ##
}

ylesanne = "Ülesanne 2.1.1"
yl = 3

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            x_test <- c(2:1, 2:1, 2:1, 4)
            y_test <- c(7, 1, 5, 2, 6, 3, 4)
            xy_test <- data.frame(x=x_test, y=y_test)
            xy1_test = xy[order(x_test,y_test),]
            xy2_test = xy[order(x_test,-y_test),]
            
            expect_equal(object = x,
                         expected = x_test,
                         info = paste0(ylesanne, ": oled 'x' väärtuseid muutnud, alusta uuesti"))
            expect_equal(object = y,
                         expected = y_test,
                         info = paste0(ylesanne, ": oled 'y' väärtuseid muutnud, alusta uuesti"))
            expect_equal(object = xy,
                         expected = xy_test,
                         info = paste0(ylesanne, ": oled andmestiku 'xy' väärtuseid muutnud, alusta uuesti"))
            
            #1
            expect_equal(object = xy1,
                         expected = xy1_test,
                         info = paste0(ylesanne, ".1: andmestik 'xy1' ei vasta oodatule"))
            
            #2
            expect_equal(object = xy2,
                         expected = xy2_test,
                         info = paste0(ylesanne, ".2: andmestik 'xy2' ei vasta oodatule"))
            
            expect_true(length(grep("order\\(", tmp_part)) >= 2, 
                        info = paste0(ylesanne, ": mõlemas ülesandes on vaja kasutada käsku 'order'"),
                        label = paste0(ylesanne, " order() funktsiooni kontroll"))
            
          })


# Ülesanne 2.2.1 õige lahendus -----
if(FALSE){
  #1
  iris.sort1 = iris[order(iris$Sepal.Width, iris$Sepal.Length,iris$Petal.Width),]
  
  #2
  #eelviimane (?) = iris.sort1$sordinimi[nrow(iris)-1] 
}

ylesanne = "Ülesanne 2.2.1"
yl = 4

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            sort1_test = iris[order(iris$Sepal.Width, iris$Sepal.Length,iris$Petal.Width),]
            expect_equal(object = iris.sort1,
                         expected = sort1_test,
                         info = paste0(ylesanne, ".1: andmestiku 'iris.sort1' sisu ei ole korrektne"))
            
            expect_true(length(grep("order\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada käsku 'order'"),
                        label = paste0(ylesanne, ".1 order() funktsiooni kontroll"))
            
            expect_true(length(grep("\\(.+,.+,.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: antud ülesandes peab sorteerimisreegli funktsioonil olema kolm argumenti"),
                        label = paste0(ylesanne, ".1 funktsiooni argumentide kontroll"))
            
            #2
            #eelviimane ? 
            
          })


# Ülesanne 2.3.1 õige lahendus -----
if(FALSE){
  #1
  iris.sort2 = iris[order(iris$Sepal.Width,-iris$Sepal.Length),]
  
  #2
  #kolmekymnes (?) = iris.sort2$sordinimi[30]
}

ylesanne = "Ülesanne 2.3.1"
yl = 5

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            sort2_test = iris[order(iris$Sepal.Width,-iris$Sepal.Length),]
            expect_equal(object = iris.sort2,
                         expected = sort2_test,
                         info = paste0(ylesanne, ".1: andmestiku 'iris.sort2' sisu ei ole korrektne"))
            
            expect_true(length(grep("order\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada sorteerimisreegli funktsiooni 'order'"),
                        label = paste0(ylesanne, ".1 order() funktsiooni kontroll"))
            
            expect_true(length(grep("\\(.+,.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: antud ülesandes peab sorteerimisreegli funktsioonil olema kaks argumenti"),
                        label = paste0(ylesanne, ".1 funktsiooni argumentide kontroll"))
            
            #2
            #kolmekymnes?
            
          })



# Ülesanne 3.1.1 õige lahendus -----
if(FALSE){
  B <-  read.csv2(file = "https://github.com/Rkursus/sygis2019/raw/master/data/B.csv",   nrows = 160, stringsAsFactors = T)
  B <- B[, substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]
  
  #1
  str(B)
  library(reshape2)
  
  #2
  testid.pikk = melt(B,id.vars = 1, measure.vars = 3:42)
  
  #3
  str(testid.pikk)
}

ylesanne = "Ülesanne 3.1.1"
yl = 6

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 library() funktsiooni kontroll"))
            
            #2
            pikk_test = melt(B,id.vars = 1, measure.vars = 3:42)
            expect_equal(object = testid.pikk,
                         expected = pikk_test,
                         info = paste0(ylesanne, ".2: andmestik 'testid.pikk' ei vasta oodatule"))
            
            expect_true(length(grep("melt.+B.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: 'melt' käsu ühe argumendina on vaja täpsustada andmestik B"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("melt.+id.vars.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: argumendi 'id.vars' väärtus 'melt' käsus täpsustamata"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("melt.+measure.vars.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: argumendi 'measure.vars' väärtus 'melt' käsus täpsustamata"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
            expect_true(ncol(testid.pikk)==3, 
                        info = paste0(ylesanne, ".2: Sobiva 'id.vars' väärtuse korral peaks andmestikku tekkima 3 veergu. Tunnusenimed kirjuta selle täpsustamiseks jutumärkidesse või anna ette sobiv(ad) veeruindeks(id)."),
                        label = paste0(ylesanne, ".2 veergude kontroll"))
            
            expect_true(nrow(testid.pikk)==6400, 
                        info = paste0(ylesanne, ".2: Sobiva 'measure.vars' väärtuse korral peaks andmestikku tekkima 6400 rida. Tunnusenimed kirjuta selle täpsustamiseks jutumärkidesse või anna ette sobivad veeruindeksid."),
                        label = paste0(ylesanne, ".2 veergude kontroll"))
            
            #3
            expect_true(length(grep("str\\(testid.pikk\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: pole kasutatud funktsiooni 'str'"),
                        label = paste0(ylesanne, ".3 funktsiooni kontroll"))
            
          })


# Ülesanne 3.2.1 õige lahendus -----
if(FALSE){
  rotid <- read.csv(file.choose(), header = T, stringsAsFactors = F, 
                    colClasses = c("numeric", "character", "character", "factor"))
  summary(rotid)
  #1
  library(reshape2)
  
  #2
  rotid.lai = dcast(rotid, Rat+Diet ~ Time, value.var = "weight")
  
  #3
  rotid.lai
}

ylesanne = "Ülesanne 3.2.1"
yl = 7

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 library() funktsiooni kontroll"))
            
            #2
            lai_test = dcast(rotid, Rat+Diet ~ Time, value.var = "weight")
            expect_equal(object = rotid.lai,
                         expected = lai_test,
                         info = paste0(ylesanne, ".2: andmestik 'rotid.lai' ei vasta oodatule"))
            
            expect_true(length(grep("dcast.+rotid.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: 'dcast' käsu ühe argumendina on vaja täpsustada andmestik 'rotid'"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("dcast.+(Rat\\+Diet|Diet\\+Rat)~Time", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: ridade-veergude paigutuse valem ei ole korrektne"),
                        label = paste0(ylesanne, ".2 valemi kontroll"))
            
            expect_true(length(grep("dcast.+value.var=\"weight\"", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: 'dcast' käsu argumendi 'value.var' väärtus ei ole korrektne"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
          })


# Ülesanne 3.3.1 õige lahendus -----
if(FALSE){
  #1
  tabel1 = table(rotid$Rat)
  tabel1
  
  #2
  tabel2 = dcast(rotid, Rat~"mootmisi")
  tabel2
  
  #3
  #katkestajaid(?)=3
}

ylesanne = "Ülesanne 3.3.1"
yl = 8

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            tabel1_test = table(rotid$Rat)
            expect_equal(object = tabel1,
                         expected = tabel1_test,
                         info = paste0(ylesanne, ".1: 'tabel1' sisu ei vasta oodatule"))
            
            expect_true(length(grep("table\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: pole kasutatud funktsiooni 'table'"),
                        label = paste0(ylesanne, ".1 table() funktsiooni kontroll"))
            
            #2
            tabel2_test = dcast(rotid, Rat~"mootmisi")
            expect_equal(object = tabel2,
                         expected = tabel2_test,
                         info = paste0(ylesanne, ".2: 'tabel2' sisu ei vasta oodatule"))
            
            expect_true(length(grep("dcast.+rotid.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: 'dcast' käsu ühe argumendina on vaja täpsustada andmestik 'rotid'"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("dcast.+Rat~", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: argumendis 'formula' on reatunnus valesti määratud"),
                        label = paste0(ylesanne, ".2 valemi kontroll"))
            
            expect_true(length(grep("dcast.+~\"mootmisi\"", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: argumendis 'formula' pole mõõtmiste arv \"mootmisi\" korrektselt määratud"),
                        label = paste0(ylesanne, ".2 valemi kontroll"))
            
            #3
            #katkestajaid?
            
            
          })


# Ülesanne 3.4.1 õige lahendus -----
if(FALSE){
  #1
  tabel3 = dcast(rotid, Diet+Rat~"kaalu mediaan",value.var = "weight", fun.aggregate = median)
  tabel3
  #2
  rott2mediaan = 240.0
}

ylesanne = "Ülesanne 3.4.1"
yl = 9

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            tabel3_test = dcast(rotid, Diet+Rat~"kaalumediaan",value.var = "weight", fun.aggregate = median)
            expect_equal(object = tabel3,
                         expected = tabel3_test,
                         info = paste0(ylesanne, ".1: 'tabel3' sisu ei vasta oodatule"))
            
            expect_true(length(grep("dcast\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: pole kasutatud funktsiooni 'dcast'"),
                        label = paste0(ylesanne, ".1 dcast() funktsiooni kontroll"))
            
            expect_true(length(grep("Diet\\+Rat~\"kaalumediaan\"", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Argument 'formula' on valesti määratud. Tabelis võiks reas esimene väärtus olla dieedi number, teine roti number ja kolmas leitud mediaan veerus 'kaalu mediaan'."),
                        label = paste0(ylesanne, ".1 valemi kontroll"))
            
            expect_true(length(grep("value.var=\"weight\"", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: 'dcast' käsu argumendi 'value.var' väärtus ei ole korrektne"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("median", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: argumendi 'fun.aggregate' väärtus on vale/puudu"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            #2
            expect_true(rott2mediaan==240.0, 
                        info = paste0(ylesanne, ".2: muutuja 'rott2mediaan' väärtus on vale"),
                        label = paste0(ylesanne, ".2 'rott2mediaan' kontroll"))
            
          })

# Ülesanne 3.5.1 õige lahendus -----
if(FALSE){
  arstiabita <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/eisaanud-arstiabi.txt", 
                           sep = "\t", dec = ",", header = T, check.names = FALSE)
  #1
  arstiabita
  
  #2
  pikk = melt(arstiabita,variable.name = "Aasta")
  pikk
  
  #3
  transponeeritud = dcast(pikk, Aasta~Arstiabiliik)
  transponeeritud
}

ylesanne = "Ülesanne 3.5.1"
yl = 10

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            pikk_test = melt(arstiabita,variable.name = "Aasta")
            expect_equal(object = pikk,
                         expected = pikk_test,
                         info = paste0(ylesanne, ".1: andmestik 'pikk' ei vasta oodatule"))
            
            expect_true(length(grep("variable.name=\"Aasta\"", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: argumendi 'variable.name' väärtus on vale"),
                        label = paste0(ylesanne, ".1 funktsiooni argumendi kontroll"))
            
            #2
            transponeeritud_test = dcast(pikk, Aasta~Arstiabiliik)
            expect_equal(object = transponeeritud,
                         expected = transponeeritud_test,
                         info = paste0(ylesanne, ".2: andmestik 'transponeeritud' ei vasta oodatule"))
            
            expect_true(length(grep("Aasta~Arstiabiliik", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: argumendi 'formula' väärtus on vale"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi kontroll"))
            
          })


# Ülesanne 3.6.1 õige lahendus -----
if(FALSE){
  #1
  transponeeritud = recast(arstiabita, variable~Arstiabiliik)
  transponeeritud
}

ylesanne = "Ülesanne 3.6.1"
yl = 11

test_that(ylesanne,
          {
          tmp_part = tmp_parts[[yl]]
          eval(parse(text = paste(tmp_part, collapse = '\n')))
          
          #1
          transponeeritud_test = recast(arstiabita, variable~Arstiabiliik)
          expect_equal(object = transponeeritud,
                       expected = transponeeritud_test,
                       info = paste0(ylesanne, ": andmestik 'transponeeritud' ei vasta oodatule"))
          
          expect_true(length(grep("recast.+arstiabita", tmp_part)) >0, 
                      info = paste0(ylesanne, ": 'recast' käsu ühe argumendina on vaja täpsustada andmestik 'arstiabita'"),
                      label = paste0(ylesanne, " funktsiooni argumendi kontroll"))
          
          expect_true(length(grep("variable~Arstiabiliik", tmp_part)) >0, 
                      info = paste0(ylesanne, ": argumendi 'formula' väärtus on vale"),
                      label = paste0(ylesanne, " funktsiooni argumendi kontroll"))
          

          })

