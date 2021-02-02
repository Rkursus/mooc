###%Ülesanne 1.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
head(A)
#1
install.packages("dplyr")
library(dplyr)

#2
A1 <- mutate(A, kmi= kaal/(kasv/100)^2,
             kaalugrupp = ifelse(kmi <= 25,
               "ala voi normkaal", "ylekaal"))

#3
str(A1)

###%Ülesanne 2.1 lahendus

#1
tabel <- (A1 %>% group_by(sugu, elukoht) 
            %>% summarise(n = n(),
              kesk.vanus = mean(vanus),
              kesk.kmi = mean(kmi),
              visiit.osak = sum(visiit == TRUE)/n))
tabel

#2
#sugu = 1, elukoht = 0

###%Ülesanne 3.1 lahendus
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
names(B)
dim(B)

#1
B1 <- (B %>% select(starts_with("test")))

#2
library(reshape2)
tabel <- (B1 %>% 
  melt()  %>%  
 group_by(variable)  %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max")))
arrange(tabel, variable)

###%Ülesanne 4.1 lahendus
antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")
str(antropo)
str(mass)

#1
mass_char <- (mass %>% mutate_all(.predicate= is.factor(TRUE), .funs = as.character()))

#2
uus_funktsioon <- function(x) x/10
antropo_cm_kg <- (antropo  %>%  mutate_at(vars(-SEX), uus_funktsioon))

###%Ülesanne 5.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

#1
A1 <- A %>% mutate_if(is.factor, as.character)
B1 <- B %>% mutate_if(is.factor, as.character)

#2
AB1 <- B1 %>% semi_join(A1, by="id")

#3
AB2 <- A1 %>% left_join(B1, by="id")

###%Ülesanne 6.1 lahendus
#1
install.packages("data.table")
library("data.table")
class(A1)
A1 <- as.data.table(A1)

#2
tabel1 <- A1[c(vanus > 50, kaal > 80), c(kmi, sirutus)]
A1 <- mutate(A, kmi= kaal/(kasv/100)^2,
             kaalugrupp = ifelse(kmi <= 25,
                                 "ala voi normkaal", "ylekaal"))

#3
tabel2 <- A1[visiit==FALSE,c("kesk.vanus", "kesk.pikkus"):=.(mean(vanus), mean(kasv)), by = c("sugu", "elukoht")]
tabel2

###%Ülesanne 7.1 lahendus
install.packages("curl")
library("curl")
tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] 

#1
is.data.table(tekstid)

#2
tekstid[,loigunr := as.integer(loigunr)]

#3
valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"), .N, by=hinnang]

###%Ülesanne 8.1 lahendus
library("stringr")

#1
esineb <- str_detect(tekstid$tekst, "[Ee]sti")
esineb  
#2
sagedustabel <- table(tekstid$hinnang, esineb)
sagedustabel

#3
tinglikjaotus <- prop.table(sagedustabel)
tinglikjaotus

###%Ülesanne 9.1 lahendus
library(nlme)
apelsinid <- as.data.frame(Orange)
head(apelsinid)

#1
apelsinid$kuupaev <- as.Date(apelsinid$age, "1968-12-31")

#2
ajad <- unique(apelsinid$kuupaev)
ajad

#3
nadalad <- difftime(ajad,ajad, units = "weeks")
nadalad

###%Ülesanne 10.1 lahendus
haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))
str(haigla)

#1
vead1 <- subset(haigla, )
vead1

#2
vead2 <- subset(haigla, is.na(haiglast.kp))
vead2
