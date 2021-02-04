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
            
            # 1 
            expect_true(exists("mass_char"),
                        info = paste0(ylesanne, ".1 Andmestikku `mass_char` pole tekitatud!"),
                        label = paste0(ylesanne, ".1 Andmestikku mass_char nime kontroll"))
            
            expect_true(length(grep("mutate_if\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Kasuta teises ülesandes funktsiooni `mutate_if()`"),
                        label = paste0(ylesanne, ".1 mutate_if() funktsiooni kontroll"))
            
            #esimene argument is.factor
             expect_true(length(grep("mutate_if\\(.*[^is.factor$]", tmp_part)) >0, 
                         info = paste0(ylesanne, ".1: Funktsiooni `mutate_if()` esimeseks argumendiks peab sattuma andmestik `mass`, st see saadetakse aheldamisega funktsiooni esimeseks argumendiks.
                                       Ära kustuta, ega muuda seda kohta koodist."),
                         label = paste0(ylesanne, ".1 mutate_if() argumentide kontroll"))
             
            #kas mutate_if lõpus as.character(), võiks olla ka komade arvu kontroll
            # expect_true(length(grep("mutate_if\\(.+\n.+[^as.character()$],", tmp_part)) >0, 
            #             info = paste0(ylesanne, ".1: Peab määrama teisenduse, mida valitud veergudele rakendada, see peaks veeru tüübiks määrama `character`. Kasuta funktsiooni `as.character()`."),
            #             label = paste0(ylesanne, ".1 as.character() funktsiooni kontroll"))
            
             # 2
             
             expect_true(exists("uus_funktsioon"),
                         info = paste0(ylesanne, ".2 Funktsiooni `uus_funktsioon` pole tekitatud!"),
                         label = paste0(ylesanne, ".2 Funktsiooni uus_funktsioon nime kontroll"))
             
             expect_true(length(grep("function\([^,]\)", tmp_part)) >0,
                         info = paste0(ylesanne, ".2 Piisab, kui defineeritaval funkstioonil on üks argument."),
                         label = paste0(ylesanne, ".2 Funktsiooni uus_funktsioon argumendi kontroll"))
             
             expect_true(uus_funktsioon(100)==10,
                         info = paste0(ylesanne, ".2 Funktsioon `uus_funktsioon` tagastab argumendiga `100` vale väärtuse."),
                         label = paste0(ylesanne, ".2 Funktsiooni uus_funktsioon sisu kontroll"))
             
             expect_true(sum(str_count(tmp_part,"%>%")) > 1 , 
                         info = paste0(ylesanne, ".2: Kasuta aheldamisoperaatorit `%>%` vähemalt 2 korda."),
                         label = paste0(ylesanne, ".2 %>% funktsiooni kasutamise kontroll"))
             
             expect_true(length(grep("mutate_at\\(", tmp_part)) >0, 
                         info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `mutate_at()`"),
                         label = paste0(ylesanne, ".2 mutate_at() funktsiooni kontroll"))
             
             #kontroll, kas on argument KOMA argument, töötab regex101, siin mitte
             # expect_true(length(grep("mutate_at\\([^,]*,{1}[^EX),]*.\\)", tmp_part)) >0, 
             #             info = paste0(ylesanne, ".2: Ära muuda funktsiooni `mutate_at()` etteantud argumentide väärtusi või kirjapilti."),
             #             label = paste0(ylesanne, ".2 mutate_at() argumentide kontroll"))
             
             expect_true(exists("antropo_cm_kg"),
                         info = paste0(ylesanne, ".2 Andmestikku `antropo_cm_kg` pole tekitatud!"),
                         label = paste0(ylesanne, ".2 Andmestiku antropo_cm_kg nime kontroll"))
             
             expect_true(ncol(antropo_cm_kg)==9,
                         info = paste0(ylesanne, ".2 Andmestikus `antropo_cm_kg` on mõni veerg puudu! "),
                         label = paste0(ylesanne, ".2 Andmestiku antropo_cm_kg veergude arvu kontroll"))
             
             expect_true(round(sum(antropo_cm_kg$WEIGHT, na.rm = T),2)==276164.5,
                         info = paste0(ylesanne,".2: veeru 'WEIGHT' väärtused valesti leitud"),
                         label = paste0(ylesanne,".2: veeru 'WEIGHT' väärtuste kontroll"))
             
             #esimene millimeetrites rida
             expect_true(round(sum(antropo_cm_kg$ACROMION_HT),2)==550351.2,
                         info = paste0(ylesanne,".2: veeru 'ACROMION_HT' väärtused valesti leitud"),
                         label = paste0(ylesanne,".2: veeru 'ACROMION_HT' väärtuste kontroll"))
             
            
          })


# Ülesanne 5.1 õige lahendus -----
if(FALSE){
  A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
  B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
  B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]
  
  # 1
  A1 <- A %>% mutate_if(.predicate = is.factor, .funs = as.character())
  B1 <- B %>% mutate_if(.predicate = is.factor, .funs = as.character())
  
  # 2
  AB1 <- B1 %>% semi_join(A1, by = "id")
  
  # 3
  AB2 <- A1 %>% left_join(B1, by = "id")
}

ylesanne = "Ülesanne 5.1"
yl = 5

test_that(ylesanne, 
          {
            
            A_kt <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
            B_kt <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
            B_kt <- B_kt[, c("id", "grupp", sort(names(B_kt)[-(1:2)]))]
            
            
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1 
            #dim(A)==45*8 -> väärtusi=45*8=360
            expect_true(sum(A==A_kt)==360,
                        info = paste0(ylesanne, ".1 Ära muuda andmestiku `A` sisu."),
                        label = paste0(ylesanne, ".1 Andmestiku A sisu kontroll"))
            #dim(B)==160*79 -> väärtusi=160*79=12640
            expect_true(sum(B==B_kt)==12640,
                        info = paste0(ylesanne, ".1 Ära muuda andmestiku `B` sisu."),
                        label = paste0(ylesanne, ".1 Andmestiku B sisu kontroll"))
            
            expect_true(exists("A1"),
                        info = paste0(ylesanne, ".1 Andmestikku `A1` pole tekitatud!"),
                        label = paste0(ylesanne, ".1 Andmestiku A1 nime kontroll"))
            
            expect_true(exists("B1"),
                        info = paste0(ylesanne, ".1 Andmestikku `B1` pole tekitatud!"),
                        label = paste0(ylesanne, ".1 Andmestiku B1 nime kontroll"))
            
            expect_true(length(grep("mutate_if\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Muuda `mutate_if()` käsus argumente, see pole praegu õige."),
                        label = paste0(ylesanne, ".1 mutate_if() argumentide kontroll"))
            
            expect_true(length(grep("mutate_if\\([^,]*,{1}[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Kasuta teises ülesandes funktsiooni `mutate_if()`"),
                        label = paste0(ylesanne, ".1 mutate_if() funktsiooni kontroll"))
            # 2
            
            expect_true(exists("AB1"),
                        info = paste0(ylesanne, ".2 Andmestikku `AB1` pole tekitatud!"),
                        label = paste0(ylesanne, ".2 Andmestiku AB1 nime kontroll"))
            
            expect_true(length(grep("semi_join\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kasuta teises ülesandes funktsiooni `semi_join()`"),
                        label = paste0(ylesanne, ".2 semi_join() funktsiooni kontroll"))
            
            expect_true(sum(str_count(tmp_part,"B1%>%semi_join")) > 0, 
                        info = paste0(ylesanne, ".2: Muuda `semi_join()` käsus läbi aheldamise saadetav andmestik."),
                        label = paste0(ylesanne, ".2 semi_join() argumendi kontroll"))
            
            expect_true(length(grep("semi_join\\(A1", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Muuda `semi_join()` käsus teine liidetav andmestik."),
                        label = paste0(ylesanne, ".2 semi_join() argumendi kontroll"))
            
            expect_true(length(grep("semi_join\\([^,]*,{1}[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Funktsioonis `semi_join()` liiga palju argumente."),
                        label = paste0(ylesanne, ".2 semi_join() argumentide kontroll"))
            
            expect_true(sum(dim(AB1)==c(35,79))==2,
                        info = paste0(ylesanne, ".2: Andmestikus `AB1` on mõni veerg ja/või rida puudu!"),
                        label = paste0(ylesanne, ".2 Andmestiku AB1 dimensioonide kontroll"))
            
            # 3 
            
            expect_true(exists("AB2"),
                        info = paste0(ylesanne, ".3 Andmestikku `AB2` pole tekitatud!"),
                        label = paste0(ylesanne, ".3 Andmestiku AB2 nime kontroll"))
            
            expect_true(length(grep("left_join\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Kasuta kolmandas ülesandes funktsiooni `left_join()`"),
                        label = paste0(ylesanne, ".3 left_join() funktsiooni kontroll"))
            
            expect_true(sum(str_count(tmp_part,"A1%>%left_join")) > 0, 
                        info = paste0(ylesanne, ".2: Muuda `left_join()` käsus läbi aheldamise saadetav andmestik."),
                        label = paste0(ylesanne, ".2 left_join() argumendi kontroll"))
            
            expect_true(length(grep("left_join\\(B1", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Muuda `left_join()` käsus teine liidetav andmestik."),
                        label = paste0(ylesanne, ".3 left_join() argumendi kontroll"))
            
            expect_true(length(grep("left_join\\([^,]*,{1}[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Funktsioonis `left_join()` liiga palju argumente."),
                        label = paste0(ylesanne, ".3 left_join() argumentide kontroll"))
            
            expect_true(sum(dim(AB2)==c(45,86))==2,
                        info = paste0(ylesanne, ".3: Andmestikus `AB2` on mõni veerg ja/või rida puudu!"),
                        label = paste0(ylesanne, ".3 Andmestiku AB2 dimensioonide kontroll"))
            
          })


# Ülesanne 6.1 õige lahendus -----
if(FALSE){
  library(data.table)
  A <- as.data.table(A)
  
  # 2
  tabel1 <- A[vanus > 50 & kaal > 80, 
              .(kmi = kaal / (kasv / 100) ** 2, sirutus)]
  tabel1
  
  # 3
  tabel2 <- A[visiit == FALSE, 
              .(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv)),
              by = .(sugu, elukoht)]
  tabel2
}

ylesanne = "Ülesanne 6.1"
yl = 6

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1 
            expect_true(length(grep("library\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 library() funktsiooni kontroll"))
            
            expect_true(length(grep("library\\(data.table", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Käsu `library()` argumendiks anna paketi nimi `reshape2`"),
                        label = paste0(ylesanne, ".1 library() argumendi kontroll"))
            
            expect_true(sum(str_count(tmp_part,"library\\(data\\.table\\)")) > 0, 
                        info = paste0(ylesanne, ".1: Käsu `library()` argumendiks anna paketi nimi `data.table`"),
                        label = paste0(ylesanne, ".1 library() argumendi kontroll"))
            
            expect_true(sum(str_count(tmp_part,"as\\.data\\.table\\(A\\)")) > 0, 
                        info = paste0(ylesanne, ".1: Andmestiku A tüüp tuleb muuta tüübiks `data.table`"),
                        label = paste0(ylesanne, ".1 library() argumendi kontroll"))
            
            # 2
            expect_true(exists("tabel1"),
                        info = paste0(ylesanne, ".2 Andmestikku `tabel1` pole tekitatud!"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel1 nime kontroll"))
            
            expect_true(sum(dim(tabel1)==c(11,2))==2,
                        info = paste0(ylesanne, ".2: Andmestikus `tabel1` on mõni veerg ja/või rida puudu!"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel1 dimensioonide kontroll"))
            
            expect_true(sum(colnames(tabel1)==c("kmi","sirutus"))==2,
                        info = paste0(ylesanne, ".2: Andmestikus `tabel1` on mõni veerg valesti nimetatud"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel1 veerunimede kontroll"))
            
            expect_true(round(sum(tabel1$kmi))==294,
                        info = paste0(ylesanne, ".2: Andmestikus `tabel1` on veerg `kmi` valesti leitud"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel1 veeru kmi kontroll"))
            
            expect_true(round(sum(tabel1$sirutus))==1991,
                        info = paste0(ylesanne, ".2: Andmestikus `tabel1` on veerg `sirutus` valesti leitud"),
                        label = paste0(ylesanne, ".2 Andmestiku tabel1 veeru sirutus kontroll"))
            
            expect_true(length(grep("^tabel1$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^print(tabel1)$", tmp_parts[[yl]])) > 0 |
                          length(grep("^;print(tabel1)$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^; print(tabel1)$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".2: muutujat 'tabel1' pole välja prinditud"),
                        label = paste0(ylesanne, ".2 muutuja 'tabel1' väljatrüki kontroll"))
            
            # 3
            
            expect_true(exists("tabel2"),
                        info = paste0(ylesanne, ".3 Andmestikku `tabel2` pole tekitatud!"),
                        label = paste0(ylesanne, ".3 Andmestiku tabel2 nime kontroll"))
            
            expect_true(sum(dim(tabel2)==c(4,4))==2,
                        info = paste0(ylesanne, ".3: Andmestikus `tabel2` on mõni veerg ja/või rida puudu!"),
                        label = paste0(ylesanne, ".3 Andmestiku tabel2 dimensioonide kontroll"))
            
            expect_true(sum(colnames(tabel2)==c("sugu","elukoht","kesk.vanus","kesk.pikkus"))==4,
                        info = paste0(ylesanne, ".3: Andmestikus `tabel2` on mõni veerg valesti nimetatud"),
                        label = paste0(ylesanne, ".3 Andmestiku tabel2 veerunimede kontroll"))
            
            expect_true(round(sum(tabel2$kesk.vanus),2)==194.16,
                        info = paste0(ylesanne, ".3: Andmestikus `tabel2` on veerg `kesk.vanus` valesti leitud"),
                        label = paste0(ylesanne, ".3 Andmestiku tabel2 veeru kesk.vanus kontroll"))
            
            expect_true(round(sum(tabel2$kesk.pikkus),2)==721.33,
                        info = paste0(ylesanne, ".3: Andmestikus `tabel2` on veerg `kesk.pikkus` valesti leitud"),
                        label = paste0(ylesanne, ".3 Andmestiku tabel2 veeru kesk.pikkus kontroll"))
            
            expect_true(length(grep("^tabel2$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^print(tabel2)$", tmp_parts[[yl]])) > 0 |
                          length(grep("^;print(tabel2)$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^; print(tabel2)$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".3: muutujat 'tabel2' pole välja prinditud"),
                        label = paste0(ylesanne, ".3 muutuja 'tabel2' väljatrüki kontroll"))
            
          })


# Ülesanne 7.1 õige lahendus -----
if(FALSE){
  tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                   col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
  tekstid <- tekstid[loigunr != "None", ]
  
  # 1
  is.data.table(tekstid)
  
  # 2
  tekstid[, loigunr := as.integer(loigunr)]
  
  # 3
  valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"), 
                   .(mitu = length(loigunr)), by = hinnang];valik
}

ylesanne = "Ülesanne 7.1"
yl = 7

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1 
            
            expect_true(length(grep("is\\.data\\.table\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'is.data.table'"),
                        label = paste0(ylesanne, ".1 is.data.table() funktsiooni kontroll"))
            
            expect_true(length(grep("is\\.data\\.table\\(tekstid", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Funktsiooni `is.data.table()` argumendiks on vale andmestik."),
                        label = paste0(ylesanne, ".1 is.data.table() argumendi kontroll"))
            
            expect_true(length(grep("is\\.data\\.table\\([^,]*\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Funktsiooni `is.data.table()` argumendiks läheb ainult andmestik."),
                        label = paste0(ylesanne, ".1 is.data.table() argumendi kontroll"))
            
            # 2
            
            expect_true(length(grep("as\\.integer", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Teises ülesandes kasuta funktsiooni `as.integer()`"),
                        label = paste0(ylesanne, ".2 as.integer() funktsiooni kontroll"))
            
            expect_true(length(grep("as\\.integer\\([^,]*\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Funktsiooni `as.integer()` argumendiks läheb ainult teisendatava tunnuse nimi."),
                        label = paste0(ylesanne, ".2 as.integer() argumendi kontroll"))
            
            expect_true(length(grep("loigunr\\:\\=", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kontrolli, kas teed teisenduse olemasoleva andmestiku sees st kas kasutad operaatorit `:=`."),
                        label = paste0(ylesanne, ".2 operaatori `:=` kontroll"))
            
            expect_true(length(grep(",loigunr", tmp_part)) >0, 
                        info = paste0(ylesanne, ".2: Kontrolli, kas panid teisenduse kirja `j`-pesasse."),
                        label = paste0(ylesanne, ".2 argumendi asukoha kontroll"))
            
            expect_true(exists("tekstid"),
                        info = paste0(ylesanne, ".2 Andmestik `tekstid` on kadunud! Alusta uuesti."),
                        label = paste0(ylesanne, ".2 Andmestiku tekstid kontroll"))
            
            expect_true("loigunr" %in% names(tekstid) ,
                        info = paste0(ylesanne, ".2 Veerg tekstilõigu numbriga on andmestikust kadunud."),
                        label = paste0(ylesanne, ".2 Veeru loigunr kontroll"))
            
            expect_true(typeof(tekstid$loigunr) =="integer",
                        info = paste0(ylesanne, ".2 Veeru `loigunr` väärtus on vale."),
                        label = paste0(ylesanne, ".2 Veeru loigunr tüübi kontroll"))
            # 3
            
            expect_true(exists("valik"),
                        info = paste0(ylesanne, ".3 Andmestikku `valik` pole! Alusta uuesti."),
                        label = paste0(ylesanne, ".3 Andmestiku valik kontroll"))
            
            expect_true(sum(dim(valik)==c(4,2))==2,
                        info = paste0(ylesanne, ".3: Andmestikus `valik` on mõni veerg ja/või rida puudu!"),
                        label = paste0(ylesanne, ".3 Andmestiku valik dimensioonide kontroll"))
            
            expect_true(sum(colnames(valik)==c("hinnang","mitu"))==2,
                        info = paste0(ylesanne, ".3: Andmestikus `valik` on mõni veerg valesti nimetatud"),
                        label = paste0(ylesanne, ".3 Andmestiku valik veerunimede kontroll"))
            
            expect_true(sum(valik$mitu)==35,
                       info = paste0(ylesanne, ".3: Andmestikus `valik` on mingid väärtused valed."),
                       label = paste0(ylesanne, ".3 Andmestiku valik veeru mitu kontroll"))
            
            expect_true(length(grep("by\\=hinnang", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Kontrolli, kas panid  `by`-pesasse kirja grupeeriva tunnuse nime. Jutumärke selle tunnuse nime ümber pole vaja!"),
                        label = paste0(ylesanne, ".3 by-pesa kontroll"))
            
            expect_true(length(grep("length(loigunr)", tmp_part)) >0 | 
                          length(grep("\\.N", tmp_part)) >0 , 
                        info = paste0(ylesanne, ".3: Pole kokku tekstid loetud "),
                        label = paste0(ylesanne, ".3 teise veeru leidmise kontroll"))
          })


# Ülesanne 8.1 õige lahendus -----
if(FALSE){
  library(stringr)
  
  # 1
  esineb <- str_detect(tekstid$tekst, pattern = "[Ee]sti")
  # või ka
  esineb.alt <- str_count(tekstid$tekst, pattern = "[Ee]esti") > 0 
  
  # 2
  sagedustabel <- table(tekstid$hinnang[esineb])
  sagedustabel
  
  # 3
  tinglikjaotus <- prop.table(sagedustabel)
  tinglikjaotus
}

ylesanne = "Ülesanne 8.1"
yl = 8

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1 
            expect_true(length(grep("str_detect", tmp_part)) >0 |
                          length(grep("str_count", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Esimeses ülesandes saab kasutada funktsiooni `str_detect` või `str_count`"),
                        label = paste0(ylesanne, ".1 `str_detect` või `str_count` funktsiooni kontroll"))
            
            expect_true(length(grep("str_detect\\([^,]*,{1}[^,]*.\\)", tmp_part)) >0 | 
                          length(grep("str_count\\([^,]*,{1}[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Funktsioonis liiga palju argumente."),
                        label = paste0(ylesanne, ".1 str_detect()/ str_count() argumentide kontroll"))
            
            expect_true(length(grep("str_count\\(tekstid\\$tekst,[^,]*.\\)", tmp_part)) >0 | 
                          length(grep("str_detect\\(tekstid\\$tekst,[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Vaja esimeseks argumendiks panna tekstilõikude vektor."),
                        label = paste0(ylesanne, ".1 str_detect()/ str_count() 1. argumendi kontroll"))
            
            expect_true(length(grep("str_count\\([^,]*,[^,]*\\[Ee\\]sti", tmp_part)) >0 | 
                          length(grep("str_detect\\([^,]*,[^,]*\\[Ee\\]sti", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Vaja teiseks argumendiks panna otsitav string."),
                        label = paste0(ylesanne, ".1 str_detect()/ str_count() 2. argumendi kontroll"))
            
            expect_true(exists("esineb"),
                        info = paste0(ylesanne, ".1: Muutuja  `esineb` on defineerimata."),
                        label = paste0(ylesanne, ".1 Muutuja esineb kontroll"))
            
            # 2
            
            expect_true(exists("sagedustabel"),
                        info = paste0(ylesanne, ".2: Andmestik  `sagedustabel` on defineerimata."),
                        label = paste0(ylesanne, ".2 Andmestik sagedustabel kontroll"))
            
            expect_true(length(grep("table\\(", tmp_part)) >0,
                        info = paste0(ylesanne, ".2: Teises ülesandes tuleb kasutada funktsiooni `table`."),
                        label = paste0(ylesanne, ".2 `table` funktsiooni kontroll"))
            
            expect_true(length(grep("^sagedustabel$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^print(sagedustabel)$", tmp_parts[[yl]])) > 0 |
                          length(grep("^;print(sagedustabel)$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^; print(sagedustabel)$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".2: muutujat 'sagedustabel' pole välja prinditud"),
                        label = paste0(ylesanne, ".2 muutuja 'sagedustabel' väljatrüki kontroll"))
            
            expect_true(sum(sagedustabel==c(167,48,65,86))==4,
                        info = paste0(ylesanne, ".2: Tabeli  `sagedustabel` väärtus ei ole korrektne. Proovi uuesti."),
                        label = paste0(ylesanne, ".2 Sagedustabeli väärtuste kontroll"))
            
            
            # 3
            expect_true(exists("tinglikjaotus"),
                        info = paste0(ylesanne, ".3: Andmestik  `tinglikjaotus` on defineerimata."),
                        label = paste0(ylesanne, ".3 Andmestik tinglikjaotus kontroll"))
            
            expect_true(length(grep("prop\\.table\\(", tmp_part)) >0,
                        info = paste0(ylesanne, ".3: Kolmandas ülesandes pead kasutama funktsiooni `prop.table`."),
                        label = paste0(ylesanne, ".3 `prop.table` funktsiooni kontroll"))
            
            expect_true(length(grep("^tinglikjaotus$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^print(tinglikjaotus)$", tmp_parts[[yl]])) > 0 |
                          length(grep("^;print(tinglikjaotus)$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^; print(tinglikjaotus)$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".3: muutujat 'tinglikjaotus' pole välja prinditud"),
                        label = paste0(ylesanne, ".3 muutuja 'tinglikjaotus' väljatrüki kontroll"))
            
            expect_true(sum(round(tinglikjaotus,2)==c(0.46,0.13,0.18,0.23))==4,
                        info = paste0(ylesanne, ".2: Tabeli  `tinglikjaotus` väärtus ei ole korrektne. Proovi uuesti."),
                        label = paste0(ylesanne, ".2 Tabeli tinglikjaotus väärtuste kontroll"))
          })


# Ülesanne 9.1 õige lahendus -----
if(FALSE){
  # Vaata andmestikku
  head(apelsinid)
  
  # Ülesanne 1: Lisa kuupäeva tunnus andmestikku
  apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31")
  
  
  # Ülesanne 2: Moodusta unikaalsete väärtuste vektor
  ajad <- unique(apelsinid$kuupaev)
  
  
  # Ülesanne 3: Kui pikk on mõõtmistevaheline aeg nädalates?
  nadalad <- difftime(ajad[-1], ajad[-length(ajad)], units = "weeks");nadalad
  nadalad
}

ylesanne = "Ülesanne 9.1"
yl = 9

test_that(ylesanne, 
          {
            library(lubridate)
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
            
            # 1
            
            expect_true(length(grep("as\\.Date\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'as.Date'"),
                        label = paste0(ylesanne, ".1 as.Date() funktsiooni kontroll"))
            
            expect_true(length(grep("as\\.Date\\([^,]*,{1}[^,]*\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Funktsioonis 'as.Date' vale arv argumente"),
                        label = paste0(ylesanne, ".1 as.Date() argumentide kontroll"))
            
            expect_true(length(grep("as\\.Date\\(apelsinid\\$age,[^,]*.\\)", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Oled `as.Date` esimeseks argumendiks vale tunnuse andnud."),
                        label = paste0(ylesanne, ".1 as.Date 1. argumendi kontroll"))
            
            expect_true(length(grep("as\\.Date\\([^,]*,[^,]*1968\\-12\\-31", tmp_part)) >0, 
                        info = paste0(ylesanne, ".1: Oled `as.Date`  käsus vale `origin` argumendi väärtuse määranud. Pead `as.Date` funktsiooni lisama `origin` argumendi."),
                        label = paste0(ylesanne, ".1 as.Date 2. argumendi kontroll"))
            
            expect_true(exists("apelsinid"),
                        info = paste0(ylesanne, ".1 Andmestik `apelsinid` on kadunud! Alusta uuesti."),
                        label = paste0(ylesanne, ".1 Andmestiku apelsinid kontroll"))
            
            expect_true("kuupaev" %in% names(apelsinid) ,
                        info = paste0(ylesanne, ".1 Andmestikus `apelsinid` pole veergu nimega `kuupaev`."),
                        label = paste0(ylesanne, ".1 Veeru kuupaev kontroll"))
            
            expect_true(is.Date(apelsinid$kuupaev),
                        info = paste0(ylesanne, ".1 Andmetabeli `apelsinid`  veeru `kuupaev` väärtused ei ole korrektsed. Proovi uuesti."),
                        label = paste0(ylesanne, ".1 Veeru kuupaev tüübi kontroll"))
            
            # 2 
            expect_true(exists("ajad"),
                        info = paste0(ylesanne, ".2 Muutuja  `ajad` on defineerimata."),
                        label = paste0(ylesanne, ".2 Muutuja ajad kontroll"))
            
            expect_true(sum(ajad == c("1969-04-28" ,"1970-04-29", "1970-10-26", 
                                  "1971-10-01", "1972-05-15", "1972-10-03",
                                   "1973-05-01"))==7,
                        info = paste0(ylesanne, ".2 Muutuja  `ajad` väärtus ei ole korrektne. Proovi uuesti."),
                        label = paste0(ylesanne, ".2 Muutuja ajad väärtuste kontroll"))
          # 3
            expect_true(exists("nadalad"),
                        info = paste0(ylesanne, ".3 Muutuja  `nadalad` on defineerimata."),
                        label = paste0(ylesanne, ".3 Muutuja nadalad kontroll"))
            
            expect_true(length(grep("^nadalad$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^print(nadalad)$", tmp_parts[[yl]])) > 0 |
                          length(grep("^;print(nadalad)$", tmp_parts[[yl]])) > 0 | 
                          length(grep("^; print(nadalad)$", tmp_parts[[yl]])) > 0, 
                        info = paste0(ylesanne, ".3: muutujat 'nadalad' pole välja prinditud"),
                        label = paste0(ylesanne, ".3 muutuja 'nadalad' väljatrüki kontroll"))
            
            expect_true(length(grep("difftime\\(", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Viimases ülesandes kasuta  funktsiooni `difftime`."),
                        label = paste0(ylesanne, ".3 difftime() funktsiooni kontroll"))
            
            expect_true(length(grep("difftime\\([^,].*\\,ajad\\,units", tmp_part)) >0 |
                          length(grep("difftime\\(ajad\\[-1\\],ajad\\[-length\\(ajad\\)\\]\\,units", tmp_part)) >0 |
                          length(grep("difftime\\(ajad\\[2\\:7\\],ajad\\[1\\:6\\]\\,units", tmp_part)) >0 |
                          length(grep("difftime\\(ajad\\[2\\:length\\(ajad\\)\\],ajad\\[1\\:length\\(ajad\\)\\-1\\]\\,units", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Käsus `difftime` peab olema kolm argumenti."),
                        label = paste0(ylesanne, ".3 difftime() argumentide kontroll"))
          
            expect_true(length(grep("units\\=", tmp_part)) >0, 
                        info = paste0(ylesanne, ".3: Määra `difftime` käsus argumendi `units` väärtus."),
                        label = paste0(ylesanne, ".3 difftime() argumendi units kontroll"))
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