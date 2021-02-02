###% ylesanne 1.1 lahendus, V kodutöö

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)

# 1
# install.packages("dplyr")
library(dplyr)

# 2
A1 <- mutate(A, kmi = kaal / (kasv / 100) ** 2, 
    kaalugrupp = ifelse(kmi > 25, "ylekaal", "ala voi normkaal"))

# 3
str(A1)

###% ylesanne2.1 lahendus ---

# 1
tabel <- A1 %>% group_by(sugu, elukoht) %>%
  summarise(n = n(), kesk.vanus = mean(vanus), 
  kesk.kmi = mean(kmi), visiit.osak = sum(visiit) / length(visiit))

# 2
# sugu = 1, elukoht = 0



###% ylesanne3.1 lahendus ---

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

###% ylesanne4.1 lahendus ---

antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")

# 1
mass_char <-  mass %>% mutate_if(.predicate = is.factor, 
              .funs = as.character())

# 2
uus_funktsioon <- function(x) x/10
antropo_cm_kg <- antropo %>% mutate_at(.vars = vars(-SEX), 
                .funs = uus_funktsioon)

###% ylesanne5.1 lahendus ---

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

###% ylesanne6.1 lahendus ---

# 1
#install.packages("data.table")
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

###% ylesanne7.1 lahendus ---

tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ]

# 1
is.data.table(tekstid)

# 2
tekstid[, loigunr := as.integer(loigunr)]

# 3
valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"), 
                .(mitu = length(loigunr)), by = hinnang]

###% ylesanne8.1 lahendus ---
library(stringr)

# 1
esineb <- str_detect(tekstid$tekst, pattern = "[Ee]sti")

# 2
sagedustabel <- table(tekstid$hinnang[esineb])
sagedustabel

# 3
tinglikjaotus <- prop.table(sagedustabel)
tinglikjaotus

###% ylesanne9.1 lahendus ---

library(nlme)
apelsinid <- as.data.frame(Orange)

# 1
apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31")

# 2
ajad <- unique(apelsinid$kuupaev)

# 3
nadalad <- difftime(shift(ajad, -1), ajad, units = "weeks")[-7]
nadalad

###% ylesanne10.1 lahendus ---

haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))

# 1
vead1 <- haigla[!is.na(haigla$haiglast.kp) 
                & haigla$haiglasse.kp > haigla$haiglast.kp, ]
vead1

# 2
vead2 <- haigla[is.na(haigla$haiglast.kp), ]
vead2
