library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 5 kontroll")

# Ülesanne 1.1 õige lahendus -----
if(FALSE){
  A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
  head(A)
  
  #1
  library(dplyr)
  
  #2
  A1 <- mutate(A, kmi=(kaal)/(kasv/100)**2,
               kaalugrupp=ifelse(kmi<=25, "ala voi normkaal","ylekaal"))
  
  #3
  str(A1)
}

ylesanne = "Ülesanne 1.1"
yl = 1

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            #1
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 library() funktsiooni kontroll"))
            # expect_true(length(grep('library\\("dplyr"\\).*?', tmp_part)) == "dplyr", 
            #             info = paste0(ylesanne, ".1: paketinimi kirjutatakse ilma jutumärkideta"),
            #             label = paste0(ylesanne, ".1 library() funktsiooni sisendi kontroll"))
            # expect_true(length(grep("library\\('dplyr'\\).*?", tmp_part)) == 'dplyr', 
            #             info = paste0(ylesanne, ".1: paketinimi kirjutatakse ilma jutumärkideta"),
            #             label = paste0(ylesanne, ".1 library() funktsiooni sisendi kontroll"))
            
            #2
            expect_true(ncol(A1)==10,
                        info = paste0(ylesanne,".2: tabelisse pole lisatud 2 uut veergu"),
                        label = paste0(ylesanne, ".2 veergude arvu kontroll"))
            
            expect_true(length(grep("ifelse\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: vaja kasutada funktsiooni 'ifelse'"),
                        label = paste0(ylesanne, ".2 ifelse() funktsiooni kontroll"))
            
            expect_true(sum(colnames(A1)[9:10] == c("kmi","kaalugrupp"))==2,
                        info = paste0(ylesanne,".2: ei leia veerge 'kmi' ja 'kaalugrupp'"),
                        label = paste0(ylesanne, ".2 veergude nimede kontroll"))
            
            expect_true(round(sum(A1$kmi),2)==1195.78,
                        info = paste0(ylesanne,".2: veeru 'kmi' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'kmi' väärtuste kontroll"))
            
            expect_true(sum(A1$kaalugrupp=="ylekaal")==39,
                        info = paste0(ylesanne,".2: veeru 'kaalugrupp' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'kaalugrupp' väärtuste kontroll"))
            #3
            expect_true(length(grep("str\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: vaja kasutada funktsiooni 'str'"),
                        label = paste0(ylesanne, ".3 str() funktsiooni kontroll"))
          })


# Ülesanne 2.1 õige lahendus -----
if(FALSE){
  # 1
  tabel <- A1 %>% group_by(sugu, elukoht) %>%
    summarise(n = n(), kesk.vanus = mean(vanus), 
              kesk.kmi = mean(kmi), visiit.osak = sum(visiit) / length(visiit))
  
  # 2
  # sugu = 1, elukoht = 0
  
}

ylesanne = "Ülesanne 2.1"
yl = 2

test_that(ylesanne, 
          {
            
            tmp_part = tmp_parts[[yl]]
            # 1
            #A1 tabel 2. ülesande kontrolliks
            library(stringr)
            A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
            A1 <- mutate(A, kmi=(kaal)/(kasv/100)**2,
                         kaalugrupp=ifelse(kmi<=25, "ala voi normkaal","ylekaal"))
            
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            expect_true(ncol(tabel)==6,
                        info = paste0(ylesanne,".1: tabelis ei ole 6 veergu"),
                        label = paste0(ylesanne, ".1 veergude arvu kontroll"))
            
            expect_true(length(grep("group_by\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Esimeses ülesandes peab andmestiku grupeerima enne kui hakata arvutusi tegeama."),
                        label = paste0(ylesanne, ".1 group_by() funktsiooni kontroll"))
            
            expect_true(length(grep("summarise\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'summarise' "),
                        label = paste0(ylesanne, ".1 summarise() funktsiooni kontroll"))
            
            expect_true(sum(str_count(tmp_part,"%>%")) > 1 , 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni %>% vähemalt kaks korda "),
                        label = paste0(ylesanne, ".1 %>% funktsiooni kontroll"))
            
            expect_true(sum(colnames(tabel) == c("sugu","elukoht","n","kesk.vanus","kesk.kmi","visiit.osak"))==6,
                        info = paste0(ylesanne, ".1: veerunimed erinevad oodatust või puuduvad"),
                        label = paste0(ylesanne, ".1 veerunimede kontroll"))
            
            expect_true(sum(tabel$n)==45,
                        info = paste0(ylesanne,".1: veerg 'n' valesti leitud"),
                        label = paste0(ylesanne,".1: veeru 'n' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$kesk.vanus),2)==197.01,
                        info = paste0(ylesanne,".1: veeru 'kesk.vanus' väärtused valesti leitud"),
                        label = paste0(ylesanne,".1: veeru 'kesk.vanus' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$kesk.kmi),2)==105.59,
                        info = paste0(ylesanne,".1: veeru 'kesk.kmi' väärtused valesti leitud"),
                        label = paste0(ylesanne,".1: veeru 'kesk.kmi' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$visiit.osak),2)==2.38,
                        info = paste0(ylesanne,".1: veeru 'visiit.osak' väärtused valesti leitud"),
                        label = paste0(ylesanne,".1: veeru 'visiit.osak' väärtuste kontroll"))
            
            # 2
            
            expect_true(length(grep("sugu=1,elukoht=1", tmp_part))>0,
                        info = paste0(ylesanne,".2: leitud vale grupp"),
                        label = paste0(ylesanne,".2: vastuse grupi kontroll"))
            
          })


# Ülesanne 3.1 õige lahendus -----
if(FALSE){
  B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
  
  # 1
  
  B1 <- B %>% select(starts_with("test"))
  
  # 2
  library(reshape2)
  tabel <- B1 %>% 
    melt() %>%  
    group_by(variable)  %>%  
    summarise_all(.funs = c("mean", "sd", "min", "max"))
  arrange(tabel, variable)
}

ylesanne = "Ülesanne 3.1"
yl = 3

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1
            expect_true(length(grep("select\\(", tmp_part)) >0,
                 info = paste0(ylesanne, ".1 Kasuta esimeses ülesandes funktsiooni `select()`."),
                 label = paste0(ylesanne, ".1 select() funktsiooni kontroll."))
            
            expect_true(length(grep("select\\(\\)", tmp_part)) == 0,
                        info = paste0(ylesanne, ".1 Funktsioonil `select()` puudub argument."),
                        label = paste0(ylesanne, ".1 select() argumendi kontroll"))
            
            expect_true(length(grep("starts_with\\(", tmp_part)) >0,
                        info = paste0(ylesanne, ".1 Kasuta esimeses ülesandes funktsiooni `starts_with`."),
                        label = paste0(ylesanne, ".1 starts_with() funktsiooni kontroll"))
            
            expect_true(length(grep("starts_with\\(\\)", tmp_part)) == 0,
                        info = paste0(ylesanne, ".1 Funktsioonil `starts_with` puudub argument."),
                        label = paste0(ylesanne, ".1 starts_with() argumendi kontroll"))
            
            expect_true(exists("B1"),
                        info = paste0(ylesanne, ".1 Andmestikku `B1` pole tekitatud!"),
                        label = paste0(ylesanne, ".1 Andmestiku B1 nime kontroll"))
            
            expect_true(ncol(B1)==40,
                        info = paste0(ylesanne, ".1 Andmestikus `B1` on mõni nõutud veerg puudu!"),
                        label = paste0(ylesanne, ".1 Andmestiku B1 veergude kontroll"))
              
            
            # 2 
            
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".2 library() funktsiooni kontroll"))
            
            expect_true(length(grep("library\\(reshape2", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Käsu `library()` argumendiks anna paketi nimi `reshape2`"),
                        label = paste0(ylesanne, ".2 library() argumendi kontroll"))
            
            expect_true(length(grep("melt\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `melt()`"),
                        label = paste0(ylesanne, ".2 melt() funktsiooni kontroll"))
            
            expect_true(length(grep("melt\\(\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Funktsioonile `melt()` peab aheldamise kaudu argumendiks minema andmestik `B1`. Teisi argumente antud funktsioonis pole vaja täpsustada."),
                        label = paste0(ylesanne, ".2 melt() argumendi kontroll"))
            
            expect_true(length(grep("group_by\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `group_by()`"),
                        label = paste0(ylesanne, ".2 group_by() funktsiooni kontroll"))
            
            expect_true(length(grep("group_by\\({1}.*[,].+\\)", tmp_part)) == 0, 
                        info = paste0(ylesanne, ".2: Funktsiooni `group_by()` esimeseks argumendiks peab aheldamine saatma pikas formaadis andmestiku. 
                                      Grupeerivaks tunnuseks peab minema veerg, kus on kirjas testide nimed. Funktsioonile antud liiga palju argumente."),
                        label = paste0(ylesanne, ".2 group_by() argumentide kontroll"))
            
            expect_true(length(grep("summarise_all\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `summarise_all()`."),
                        label = paste0(ylesanne, ".2 summarise_all() funktsiooni kontroll"))
            
            #kas 3 koma e pole lisatud rohkem argumente
            expect_true(length(grep("summarise_all.*[,]{1}.+[,]{1}.+[,]{1}.+\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Funktsioonide nimekiri pole sama, mis etteantud koodis. 
                                      Ära kustuta, ega muuda seda kohta koodist."),
                        label = paste0(ylesanne, ".2 summarise_all() argumentide kontroll"))
            
            expect_true(sum(str_count(tmp_part,"%>%")) > 3 , 
                        info = paste0(ylesanne, ".2: Kasuta aheldamisoperaatorit `%>%` vähemalt 4 korda."),
                        label = paste0(ylesanne, ".2 %>% funktsiooni kasutamise kontroll"))
            
            expect_true(exists("tabel"),
                        info = paste0(ylesanne, ".2 Andmestikku `tabel` pole tekitatud!"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel nime kontroll"))
            
            expect_true(ncol(tabel)==5,
                        info = paste0(ylesanne, ".2 Andmestikus `tabel` on mõni nõutud veerg puudu!"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel veergude kontroll"))
            
            expect_true(round(sum(tabel$mean),2)==799.68,
                        info = paste0(ylesanne,".2: veeru 'mean' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'mean' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$sd),2)==80.42,
                        info = paste0(ylesanne,".2: veeru 'sd' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'sd' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$min),2)==589,
                        info = paste0(ylesanne,".2: veeru 'min' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'min' väärtuste kontroll"))
            
            expect_true(round(sum(tabel$max),2)==1015.5,
                        info = paste0(ylesanne,".2: veeru 'max' väärtused valesti leitud"),
                        label = paste0(ylesanne,".2: veeru 'max' väärtuste kontroll"))
            
          })


# Ülesanne 4.1 õige lahendus -----
if(FALSE){
  antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
  mass <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")
  
  # 1
  mass_char <-  mass %>% mutate_if(.predicate = is.factor, 
                                   .funs = as.character())
  
  # 2
  uus_funktsioon <- function(x) x/10
  antropo_cm_kg <- antropo %>% mutate_at(.vars = vars(-SEX), 
                                         .funs = uus_funktsioon)
}

ylesanne = "Ülesanne 4.1"
yl = 4

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            expect_true(exists("mass_char"),
                        info = paste0(ylesanne, ".1 Andmestikku `mass_char` pole tekitatud!"),
                        label = paste0(ylesanne, ".1 Andmestiku mass_char nime kontroll"))
            
            expect_true(length(grep("mutate_if\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `mutate_if()`"),
                        label = paste0(ylesanne, ".2 mutate_if() funktsiooni kontroll"))
            
            #esimene argument is.factor
             expect_true(length(grep("mutate_if\\(.*[^is.factor$]", tmp_part)) >0, 
                         info = paste0(ylesanne, ".2: Funktsiooni `mutate_if()` esimeseks argumendiks peab sattuma andmestik `mass`, st see saadetakse aheldamisega funktsiooni esimeseks argumendiks.
                                       Ära kustuta, ega muuda seda kohta koodist."),
                         label = paste0(ylesanne, ".2 mutate_if() argumentide kontroll"))
             
            #kas mutate_if lõpus as.character(), võiks olla ka komade arvu kontroll
            # expect_true(length(grep("mutate_if\\(.+\n.+[^as.character()$],", tmp_part)) >0, 
            #             info = paste0(ylesanne, ".2: Peab määrama teisenduse, mida valitud veergudele rakendada, see peaks veeru tüübiks määrama `character`. Kasuta funktsiooni `as.character()`."),
            #             label = paste0(ylesanne, ".2 as.character() funktsiooni kontroll"))
            
             # 2
             
             expect_true(exists("uus_funktsioon"),
                         info = paste0(ylesanne, ".1 Funktsiooni `uus_funktsioon` pole tekitatud!"),
                         label = paste0(ylesanne, ".1 Andmestiku uus_funktsioon nime kontroll"))
             
             
            
            
          })


# Ülesanne 5.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 5.1"
yl = 5

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 6.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 6.1"
yl = 6

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 7.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 7.1"
yl = 7

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 8.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 8.1"
yl = 8

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 9.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 9.1"
yl = 9

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })


# Ülesanne 10.1 õige lahendus -----
if(FALSE){
  
}

ylesanne = "Ülesanne 10.1"
yl = 10

test_that(ylesanne, 
          {
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
          })