###% Ülesanne 1.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)

library(dplyr)

A1 <- mutate(A, kmi = kaal/(kasv/100)**2, kaalugrupp = ifelse(kmi <= 25,
                                                              "ala voi normkaal",
                                                              "ylekaal"))
str(A1)

###% Ülesanne 2.1 lahendus
tabel <- A1 %>%   
  group_by(sugu, elukoht) %>% 
  summarise(n=n(), kesk.vanus = mean(vanus), kesk.kmi = mean(kmi), visiit.osak = (sum(visiit == TRUE)/n))
tabel

# sugu = 1, elukoht = 0

###% Ülesanne 3.1 lahendus
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)

names(B)
dim(B)

B1 <- B %>% 
  select(starts_with("test"))

library(reshape2)
tabel <- B1 %>% 
  melt()  %>%  
  group_by(variable)  %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)

###% Ülesanne 4.1 lahendus
antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")

mass_char <-  mass %>% mutate_if(is.factor, as.character)

uus_funktsioon <- function(x) x/10
antropo_cm_kg <- antropo  %>%   mutate_at(.vars = vars(-SEX), .funs = uus_funktsioon)

###% Ülesanne 5.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

str(A)
str(B)

A1 <- A %>% mutate_if(is.factor, as.character)
B1 <- B %>% mutate_if(is.factor, as.character)

AB1 <- B1 %>% semi_join(A1)

AB2 <- A1 %>% inner_join(B1)

###% Ülesanne 6.1 lahendus
library(data.table)
class(A)

A <- as.data.table(A)
class(A)

tabel1 <- A[vanus>50 & kaal>80, .(kmi = kaal/(kasv/100)**2, sirutus)]
tabel1

tabel2 <- A[visiit == FALSE, .(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv)), by = .(sugu, elukoht)]
tabel2

###% Ülesanne 7.1 lahendus
tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ]

is.data.table(tekstid)

tekstid[, loigunr := as.numeric(loigunr)]

valik <- tekstid[loigunr>2 & startsWith(tekst, "A"), .N, by = hinnang]

###% Ülesanne 8.1 lahendus
library(stringr)
str(tekstid)

esineb <- str_detect(tekstid$tekst, "[Ee]sti")

sagedustabel <- table(tekstid$hinnang, esineb)
sagedustabel

tinglikjaotus <- prop.table(sagedustabel)
tinglikjaotus

###% Ülesanne 9.1 lahendus
library(nlme)
apelsinid <- as.data.frame(Orange)
head(apelsinid)

apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31")

ajad <- unique(apelsinid$kuupaev)
ajad

nadalad <- difftime(tail(ajad, -1), head(ajad, -1), units = "weeks")
nadalad

###% Ülesanne 10.1 lahendus
haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))
str(andmed)

vead1 <- subset(haigla, haigla$haiglasse.kp > haigla$haiglast.kp)
vead1

vead2 <- subset(haigla, is.na(haigla$haiglasse.kp) | is.na(haigla$haiglast.kp))
vead2
