###% Ülesanne 1.1.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
head(A)

install.packages("dplyr")
library(dplyr)

#Ülesanne 1.1.2 lahendus
A1 <- mutate(A, kmi=kaal/(kasv/100)^2,kaalugrupp=ifelse(kmi>25, "ylekaal", "ala või normkaal"))

#Ülesanne 1.1.3 lahendus
str(A1)

###%Ülesanne 2.1.1 lahendus
str(A1)
tabel <- A1 %>%   
  group_by(sugu, elukoht) %>% 
  summarise(n=n(),
            kesk.vanus=mean(vanus),
            kesk.kmi=mean(kmi),
            vistiit.osak=sum(visiit)/n)
tabel

#Ülesanne 2.1.2 lahendus
# sugu = 1, elukoht = 0

###%Ülesanne 3.1.1 lahendus
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
names(B)
dim(B)

B1 <- B %>% select(starts_with("test"))

#Ülesanne 3.1.2 lahendus
install.packages("reshape2")
library(reshape2)
tabel <- B1 %>% 
  melt()  %>%  
  group_by(variable)  %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)

###%Ülesanne 4.1.1 lahendus
antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")
str(mass)
str(antropo)

mass_char <-  mass %>% mutate_if(.predicate = is.factor, .funs = as.character)
str(mass_char)

uus_funktsioon <- function(tunnus){return(tunnus/10)}
antropo_cm_kg <- antropo  %>%  mutate_at(.vars = vars(-SEX), .funs = uus_funktsioon)

###%Ülesanne 5.1.1 lahendus
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]
str(A)
str(B)

A1 <- A %>% mutate_if(.predicate = is.factor, .funs = as.character)
B1 <- B %>% mutate_if(.predicate = is.factor, .funs = as.character)

#Ülesanne 5.1.2 lahendus
AB1 <- B1 %>% semi_join(A1, by="id")

#Ülesanne 5.1.3 lahendus
AB2 <- A1 %>% inner_join(B1, by = "id")

###%Ülesanne 6.1.1 lahendus
install.packages("data.table")
library(data.table)
class(A)
A <- as.data.table(A)
class(A)

#Ülesanne 6.1.2 lahendus
tabel1 <- A[vanus > 50 & kaal > 80, list(kmi=kaal/(kasv/100)^2,sirutus)]
tabel1

#Ülesanne 6.1.3 lahendus
tabel2 <- A[, list(kesk.vanus=mean(vanus),kesk.pikkus=mean(kasv) ), by=list(sugu, elukoht) ]
tabel2

###%Ülesanne 7.1.1 lahendus
install.packages("curl")
tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] # Eemaldame read, kus 'loigunr' puudub.
is.data.table(tekstid)

#Ülesanne 7.1.2 lahendus
tekstid[, loigunr:=as.integer(loigunr)]

#Ülesanne 7.1.3 lahendus
valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"),.N,by=hinnang]
valik     

###%Ülesanne 8.1.1 lahendus
library(stringr)
str(tekstid)
esineb <- str_detect(tekstid$tekst, "[Ee]sti")

#Ülesanne 8.1.2 lahendus
sagedustabel <- table(tekstid$hinnang, esineb)
sagedustabel

#Ülesanne 8.1.3 lahendus
tinglikjaotus <- prop.table(sagedustabel, margin=2)
tinglikjaotus

###%Ülesanne 9.1.1 lahendus
library(nlme)
apelsinid <- as.data.frame(Orange)
head(apelsinid)
apelsinid$kuupaev <- as.Date(apelsinid$age, format="%Y-%m-%d", origin="1968-12-31")

#Ülesanne 9.1.2 lahendus
ajad <- unique(apelsinid$kuupaev)
ajad

#Ülesanne 9.1.3 lahendus
nadalad <- difftime(ajad[1:6], ajad[-1], units="weeks")
nadalad

###%Ülesanne 10.1.1 lahendus
haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))
str(haigla)

vead1 <- haigla[haigla$haiglasse.kp > haigla$haiglast.kp, ]
vead1

#Ülesanne 10.1.2 lahendus
vead2 <- haigla[is.na(haigla$haiglast.kp),]
vead2
