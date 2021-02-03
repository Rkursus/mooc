library(testthat)

# Helpful command, then whiespace differences will not show up (assumes 'filename' is provided)
tmp_file = gsub(" ","", readLines(.submission,encoding="UTF-8"))

# Split the submission by exercises, to that previous results would not interfere
tmp_parts = split(tmp_file, cumsum(stringr::str_detect(tmp_file, "^###%")))

# Test setup name
context("Kodutöö 4 kontroll")

# Ülesanne 1.1 õige lahendus -----
if(FALSE){
  library(reshape2)
  jootraha <-tips
  jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))
  summary(jootraha)
  
  #1
  library(ggplot2)
  
  #2
  j1 <- ggplot(jootraha, aes(x = day)) + 
    geom_bar(fill = "royalblue") + 
    scale_x_discrete(name = "Day of week", 
                     limits = c("Thur", "Fri", "Sat", "Sun"), 
                     labels = c("thursday", "friday", "saturday", "sunday")) +
    ylab(label = "Counts")
  j1
}

ylesanne = "Ülesanne 1.1"
yl = 1

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            expect_true(length(grep("^library\\(ggplot2\\)$", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: vaja kasutada funktsiooni 'library'"),
                        label = paste0(ylesanne, ".1 paketi aktiveerimise kontroll"))
            #2
            #geom_bar(fill = "royalblue")
            expect_true(length(grep("geom_bar", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, ".2 geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom_.+\\(fill=(\"|\')royalblue(\"|\')\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: geom_<...> funktsiooni argument ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 geom_<...> funktsiooni argumendi kontroll"))
            
            #scale_x_discrete(name = "Day of week", 
                    #limits = c("Thur", "Fri", "Sat", "Sun"), 
                    #labels = c("thursday", "friday", "saturday", "sunday"))
            expect_true(length(grep("scale_x_discrete", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: pole kasutatud õiget scale_x_<...> funktsiooni"),
                        label = paste0(ylesanne, ".2 scale_x_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("name=(\"|\')[Dd]ayofweek(\"|\')", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: scale_<...> funktsiooni argument 'name' ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 scale_<...> funktsiooni argumendi kontroll"))
            
                                    #limits = c("Thur", "Fri", "Sat", "Sun") or =levels(jootraha$day) or =levels(jootraha[,5])
            expect_true(length(grep("limits.+(c\\((\"|\')[Tt]hur(\"|\'),(\"|\')[Ff]ri(\"|\'),(\"|\')[Ss]at(\"|\'),(\"|\')[Ss]un(\"|\')\\)|levels\\(jootraha$day\\)|levels\\(jootraha[,5]\\))", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: scale_<...> funktsiooni argument 'limits' ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 scale_<...> funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("labels=c\\((\"|\')thursday(\"|\'),(\"|\')friday(\"|\'),(\"|\')saturday(\"|\'),(\"|\')sunday(\"|\')\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: scale_<...> funktsiooni argument 'labels' ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 scale_<...> funktsiooni argumendi kontroll"))          
            
            #ylab(label = "Counts")
            expect_true(length(grep("ylab\\(label=(\"|\')[Cc]ounts(\"|\')\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: ylab() funktsiooni argumendi 'label' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 ylab() funktsiooni argumendi kontroll"))
          })


# Ülesanne 2.1 õige lahendus -----
if(FALSE){
  library(scales)
  j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
    geom_bar(position = "fill") +
    scale_y_continuous(name = "Percentage", labels = percent) + 
    scale_fill_hue(name = "Time") + 
    xlab(label = "Day")
  j1
}

ylesanne = "Ülesanne 2.1"
yl = 2

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #aes(x = day, fill = time)
            expect_true(length(grep("aes\\(x=day", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("aes\\(.+fill=time\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni teine argument ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi kontroll"))
            
            #geom_bar(position = "fill")
            expect_true(length(grep("geom_bar", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, " geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom.+\\(position=\"fill\"\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": geom_<...> funktsiooni argument ei vasta oodatule"),
                        label = paste0(ylesanne, " geom_<...> funktsiooni argumendi kontroll"))
            
            #scale_y_continuous(name = "Percentage", labels = percent)
            expect_true(length(grep("scale_y_continuous", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": y-telge kirjeldav scale_<...> funktsioon ei vasta oodatule"),
                        label = paste0(ylesanne, " scale_<...> funktsiooni kontroll"))
           
            expect_true(length(grep("scale.+name=(\"|\')[Pp]ercentage(\"|\')", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": y-telje nimi pole scale_<...> funktsioonis korrektselt määratud"),
                        label = paste0(ylesanne, " y-telje nime kontroll"))
            
            expect_true(length(grep("scale.+labels=percent", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": scale_<...> funktsiooni argument y-telje väärtuste sildistamiseks (%) ei vasta oodatule"),
                        label = paste0(ylesanne, " scale<...> funktsiooni argumendi kontroll"))
            
            #scale_fill_hue(name = "Time") või scale_color_hue või scale_colour_hue
            expect_true(length(grep("scale_(fill|color|colour)_hue", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": värvilegendi kirjeldav scale_<...>_hue funktsioon ei vasta oodatule"),
                        label = paste0(ylesanne, " scale_<...>_hue funktsiooni kontroll"))
            
            expect_true(length(grep("scale_.+_hue\\(name=(\"|\')[Tt]ime(\"|\')\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": funktsiooni scale_<...>_hue argument ei vasta oodatule"),
                        label = paste0(ylesanne, " värvilegendi pealkirja  kontroll"))
            
            #xlab(label = "Day")
            expect_true(length(grep("xlab\\(label=(\"|\')[Dd]ay(\"|\')\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": xlab funktsiooni argument ei vasta oodatule"),
                        label = paste0(ylesanne, " xlab funktsiooni argumendi kontroll"))
          })


# Ülesanne 3.1 õige lahendus -----
if(FALSE){
  #1
  j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
    geom_point(aes(color = day)) +
    scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
  
  j1
  
  #2
  suurim  <- "Sat"
}

ylesanne = "Ülesanne 3.1"
yl = 3

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            #aes(x = total_bill, y = tip)
            expect_true(length(grep("aes\\(x=total_bill", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+y=tip\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'y' kontroll"))
            
            #geom_point(aes(color = day))
            expect_true(length(grep("geom_point", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom.+\\(aes", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: geom_<...> funktsiooni argumendina pole aes() funktsiooni kasutatud"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("\\((color|colour)=day\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: geom_<...> argumendiks oleva funktsiooni aes() argument ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni aes() argumendi kontroll"))
            
            #scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
            expect_true(length(grep("scale_color_hue", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: pole kasutatud õiget scale_<...>_hue funktsiooni"),
                        label = paste0(ylesanne, ".1 scale_<...>_hue funktsiooni kontroll"))
            
            expect_true(length(grep("scale_.+_hue.+limits=", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: scale_<...>_hue funktsioonis puudub argument 'limits'"),
                        label = paste0(ylesanne, ".1 scale_<...>_hue funktsiooni argumendi kontroll"))
            
                                    #limits = c("Thur", "Fri", "Sat", "Sun") or =levels(jootraha$day) or =levels(jootraha[,5])
            expect_true(length(grep("limits.+(c\\((\"|\')[Tt]hur(\"|\'),(\"|\')[Ff]ri(\"|\'),(\"|\')[Ss]at(\"|\'),(\"|\')[Ss]un(\"|\')\\)|levels\\(jootraha$day\\)|levels\\(jootraha[,5]\\))", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: scale_<...>_hue funktsiooni argument 'limits' ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 scale_<...>_hue funktsiooni argumendi kontroll"))
            
            #2
            suurim_test = "Sat"
            expect_true(suurim==suurim_test,
                         info = paste0(ylesanne, ".2: muutuja 'suurim' väärtus ei vasta oodatule"),
                         label = paste0(ylesanne, ".2 muutuja 'suurim' kontroll"))
          })


# Ülesanne 4.1 õige lahendus -----
if(FALSE){
  #1
  j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
    geom_point(aes(color = sex, shape = smoker)) 
  j1
  
  #2
  j1 + geom_smooth(method = lm)
}

ylesanne = "Ülesanne 4.1"
yl = 4

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            #aes(x = total_bill, y = tip)
            expect_true(length(grep("aes\\(x=total_bill", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+y=tip\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'y' kontroll"))
            
            
            #geom_point(aes(color = sex, shape = smoker))
            expect_true(length(grep("geom_point", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom.+\\(aes", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: geom_<...> funktsiooni argumendina pole aes() funktsiooni kasutatud"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("(color|colour)=sex", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: geom_<...> argumendiks oleva funktsiooni aes() argument, mis määrab punktide värvi, ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni aes() argumendi kontroll"))
            
            expect_true(length(grep("shape=smoker", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: geom_<...> argumendiks oleva funktsiooni aes() argument, mis määrab punktide kuju, ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni aes() argumendi kontroll"))
            
            #2
            #j1 + geom_smooth(method = lm)
            expect_true(length(grep("geom_smooth", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: geom_smooth() funktsiooni pole kasutatud"),
                        label = paste0(ylesanne, ".2 regressioonisirge funktsiooni kontroll"))
            
            expect_true(length(grep("method=lm", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: oodatud argument geom_smooth() funktsioonis on määramata"),
                        label = paste0(ylesanne, ".2 geom_smooth() argumendi kontroll"))
            
          })


# Ülesanne 5.1 õige lahendus -----
if(FALSE){
  #1
  jootraha$ratio <- jootraha$tip/jootraha$total_bill
  
  #2
  ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins = 15)
  
  #3
  erind <- "Dinner"
}

ylesanne = "Ülesanne 5.1"
yl = 5

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            expect_true("ratio"%in%names(jootraha), 
                        info = paste0(ylesanne, ".1: uus tunnus nimega 'ratio' on andmestikku lisamata"),
                        label = paste0(ylesanne, ".1 uue tunnuse olemasolu kontroll"))
            
            ratio_test = jootraha$tip/jootraha$total_bill
            expect_equal(object = jootraha$ratio,
                         expected = ratio_test,
                         info = paste0(ylesanne, ".1: uue tunnuse 'ratio' väärtused ei ole õiged"))
            
            #2
            #aes(x = ratio, y = ..density..)
            expect_true(length(grep("aes\\(x=ratio", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: aes() funktsiooni esimene argument ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 aes() funktsiooni esimese argumendi kontroll"))
            
            #geom_histogram(bins = 15)
            expect_true(length(grep("geom_histogram", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, ".2 geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom.+\\(bins=15\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: geom_<...> funktsiooni (intervallide arvu) argument ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 geom_<...> funktsiooni argumendi kontroll"))
            
            #3
            erind_test = "Dinner"
            expect_true(erind==erind_test,
                        info = paste0(ylesanne, ".3: muutuja 'erind' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".3 muutuja 'erind' kontroll"))
            
          })


# Ülesanne 6.1 õige lahendus -----
if(FALSE){
  vastus = 3
}

ylesanne = "Ülesanne 6.1"
yl = 6

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            expect_true(vastus==3,
                        info = paste0(ylesanne, ".1: muutuja 'vastus' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 vastuse kontroll"))
          })


# Ülesanne 7.1 õige lahendus -----
if(FALSE){
  #1
  j1 <- ggplot(jootraha, aes(x = size, y = ratio, group = size)) + geom_boxplot()
  j1
  
  #2
  j1 + labs(x = "laudkonna suurus",  y = "jootraha ja arve suhe")
  
  #3
  tabel <- table(jootraha$size)
  tabel
}

ylesanne = "Ülesanne 7.1"
yl = 7

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            #aes(x = size, y = ratio, group = size)
            expect_true(length(grep("aes\\(x=size", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+y=ratio", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'y' kontroll"))
            
            expect_true(length(grep("aes.+group=size", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: aes() funktsiooni argumendi 'group' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".1 aes() funktsiooni argumendi 'group' kontroll"))
            
            #geom_boxplot()
            expect_true(length(grep("geom_boxplot", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: pole kasutatud õiget geom_<...> funktsiooni"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni kontroll"))
            
            expect_true(length(grep("geom_.+\\(\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".1: nõutud geom_<...> funktsioonil pole argumente vaja"),
                        label = paste0(ylesanne, ".1 geom_<...> funktsiooni argumendi kontroll"))
            
            #2
            #labs(x = "laudkonna suurus",  y = "jootraha ja arve suhe")
            expect_true(length(grep("labs.+x=\"laudkonnasuurus\"", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: x-telje nimi ei vasta oodatule "),
                        label = paste0(ylesanne, ".2 x-telje nime kontroll"))
            
            expect_true(length(grep("y=\"jootrahajaarvesuhe\"", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: y-telje nimi ei vasta oodatule "),
                        label = paste0(ylesanne, ".2 y-telje nime kontroll"))
            
            #3
            tabel_test = table(jootraha$size)
            expect_equal(object = tabel,
                         expected = tabel_test,
                         info = paste0(ylesanne, ".3: 'tabel' ei vasta oodatule"))
            
          })


# Ülesanne 8.1 õige lahendus -----
if(FALSE){
  #1
  jootraha$tip.per.person <- jootraha$tip/jootraha$size
  
  #2
  ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
    stat_summary(geom = "line",  fun = mean) + 
    scale_x_continuous(breaks = 1:6)
}

ylesanne = "Ülesanne 8.1"
yl = 8

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #1
            expect_true("tip.per.person"%in%names(jootraha), 
                        info = paste0(ylesanne, ".1: uus tunnus nimega 'tip.per.person' on andmestikku lisamata"),
                        label = paste0(ylesanne, ".1 uue tunnuse olemasolu kontroll"))
            
            tip.per.person_test = jootraha$tip/jootraha$size
            expect_equal(object = jootraha$tip.per.person,
                         expected = tip.per.person_test,
                         info = paste0(ylesanne, ".1: uue tunnuse 'tip.per.person' väärtused ei ole õiged"))
            
            #2
            #aes(x = size, y = tip.per.person)
            expect_true(length(grep("aes\\(x=size", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+,y=tip.per.person\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 aes() funktsiooni argumendi 'y' kontroll"))
            
            #stat_summary(geom = "line",  fun = mean) 
            expect_true(length(grep("geom=(\"|\')line(\"|\')", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: funktsiooni stat_summary() argumendi 'geom' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi 'geom' kontroll"))
            
            expect_true(length(grep("fun=mean", tmp_part)) > 0, 
                        info = paste0(ylesanne, ".2: funktsiooni stat_summary() argumendi 'fun' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, ".2 funktsiooni argumendi 'fun' kontroll"))
            
          })


# Ülesanne 9.1 õige lahendus -----
if(FALSE){
  ggplot(jootraha, aes(x = size, y = tip.per.person, color = sex)) +
    stat_summary(geom = "line",  fun = mean) + 
    scale_x_continuous(breaks = 1:6)
}

ylesanne = "Ülesanne 9.1"
yl = 9

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #aes(x = size, y = tip.per.person, color = sex)
            expect_true(length(grep("aes\\(x=size", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+,y=tip.per.person,", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi 'y' kontroll"))
            
            expect_true(length(grep("aes.+,(color|colour).+\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsioonis pole kasutatud argumenti 'color'"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("aes.+=sex\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni viimase argumendi väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni viimase argumendi kontroll"))
            
            #stat_summary(geom = "line",  fun = mean) 
            expect_true(length(grep("geom=(\"|\')line(\"|\')", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": funktsiooni stat_summary() argumendi 'geom' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " funktsiooni argumendi 'geom' kontroll"))
            
            expect_true(length(grep("fun=mean", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": funktsiooni stat_summary() argumendi 'fun' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " funktsiooni argumendi 'fun' kontroll"))
            
          })


# Ülesanne 10.1 õige lahendus -----
if(FALSE){
  ggplot(jootraha, aes(x = size, y = tip.per.person, linetype = sex)) + 
    stat_summary(geom = "line",  fun = mean) +
    scale_x_continuous(breaks = 1:6)
}

ylesanne = "Ülesanne 10.1"
yl = 10

test_that(ylesanne, 
          {
            tmp_part = tmp_parts[[yl]]
            eval(parse(text = paste(tmp_part, collapse = '\n')))
            
            #aes(x = size, y = tip.per.person, linetype = sex)
            expect_true(length(grep("aes\\(x=size", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni argumendi 'x' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi 'x' kontroll"))
            
            expect_true(length(grep("aes.+,y=tip.per.person,", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni argumendi 'y' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi 'y' kontroll"))
            
            expect_true(length(grep("aes.+,linetype.+\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsioonis pole kasutatud argumenti 'linetype'"),
                        label = paste0(ylesanne, " aes() funktsiooni argumendi kontroll"))
            
            expect_true(length(grep("aes.+=sex\\)", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": aes() funktsiooni viimase argumendi väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " aes() funktsiooni viimase argumendi kontroll"))
            
            #stat_summary(geom = "line",  fun = mean) 
            expect_true(length(grep("geom=(\"|\')line(\"|\')", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": funktsiooni stat_summary() argumendi 'geom' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " funktsiooni argumendi 'geom' kontroll"))
            
            expect_true(length(grep("fun=mean", tmp_part)) > 0, 
                        info = paste0(ylesanne, ": funktsiooni stat_summary() argumendi 'fun' väärtus ei vasta oodatule"),
                        label = paste0(ylesanne, " funktsiooni argumendi 'fun' kontroll"))
            
          })
