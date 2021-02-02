###% Lahendus: 1.1 Ülesanded-----------------------------------------

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)

# Vaata andmestik üle
head(A)

# Ülesanne 1: aktiveeri pakett
library(dplyr)


# Ülesanne 2: lisa tunnused
A1 <- mutate(A, 
             kmi = kaal / ((kasv/100)^2),
             kaalugrupp = ifelse(kmi>25, "ylekaal", "ala voi normkaal"))


# Ülesanne 3: vaata tulemust
str(A1)

###% Lahendus: 2.1 Ülesanded-----------------------------------------

# Vaata andmestik üle
str(A1)


# Ülesanne 1: leia tabel
tabel <- A1 %>%   
  group_by(sugu, elukoht) %>% 
  summarise(
    n = n(),
    kesk.vanus = mean(vanus),
    kesk.kmi = mean(kmi),
    visiit.osak = sum(visiit)/n*100 #percentage of people that went to the doctor in the group
  )
tabel

# Ülesanne 2: leia mis grupp on kõige madalama arstivisiidil käimise osakaaluga.
# sugu = 1, elukoht = 0

###% Lahendus: 3.1 Ülesanded-----------------------------------------

B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)

# Vaata üle tunnusenimed ja andmestiku dimensioonid
names(B)
dim(B)

# Ülesanne 1: Alamandmestiku valik
B1 <- B  %>% select(starts_with("test"))


# Ülesanne 2: Täienda koodi
library(reshape2)

#some testing
#test <- B1 %>% melt()
#test2 <- test %>% group_by(variable) %>% summarise(max(value))
#test3 <- B1 %>% melt() %>% group_by(variable) %>% summarise(max(value))

tabel <- B1 %>% 
  melt()  %>%  
  group_by(variable)  %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)

###% Lahendus: 4.1 Ülesanded-----------------------------------------

antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")

# Tutvu andmestikega
str(mass)
str(antropo)

# Ülesanne 1: teisenda faktorid tavaliseks tekstiks
mass_char <-  mass %>% mutate_if(.predicate = is.factor, .funs = as.character)
#not sure here, I think none of the attributes have been a factor in the first place?

# Ülesanne 2: teisenda ühikud
uus_funktsioon <- function(x) x/10
antropo_cm_kg <- antropo   %>%  mutate_at(.vars = vars(-SEX), .funs = uus_funktsioon)

###% Lahendus: 5.1 Ülesanded-----------------------------------------

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

# Vaata anmdestikud üle
str(A)
str(B)


# Ülesanne 1:  Teisenda tunnuse tüüp
A1 <- A %>% mutate_if(.predicate = is.factor, .funs = as.character)
B1 <- B %>% mutate_if(.predicate = is.factor, .funs = as.character)


# Ülesanne 2: ühenda andmestikud
AB1 <- B1 %>% semi_join(A1)
#all rows from the first table where there is a corresponding matching value in second

# Ülesanne 3: ühenda andmestikud
AB2 <- A1 %>% inner_join(B1)

###% Lahendus: 6.1 Ülesanded-----------------------------------------

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)

# Ülesanne 1: aktiveeri pakett
library(data.table)
class(A) # Mis tüüpi on andmestik A?
#data.frame

A <- as.data.table(A)
class(A) # Mis tüüpi nüüd?
#data.table data.frame

# Ülesanne 2: tee valik objektidest ja vali/arvuta tunnused
tabel1 <- A[vanus > 50 & kaal > 80, .(sirutus, kmi = kaal/((kasv/100)^2))]
tabel1

# Ülesanne 3: tee valik objektidest ja vali/arvuta tunnused gruppide kaupa
tabel2 <- A[visiit == F, .(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv)), by = list(sugu, elukoht)]
tabel2

###% Lahendus: 7.1 Ülesanded-----------------------------------------

tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] # Eemaldame read, kus 'loigunr' puudub.

# Ülesanne 1: kontrolli, kas andmestik on data.table-tüüpi
class(tekstid) # jah

# Ülesanne 2: tee tüübiteisendus
tekstid[,loigunr := as.integer(loigunr)]


# Ülesanne 3: vali read ja leia sagedused (ära pane veergudele uusi nimesid)
valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"), .N, by = hinnang]
#no idea if this is what you meant but it took me some time to find the solution, namely the ".N"

###% Lahendus: 8.1 Ülesanded-----------------------------------------

library(stringr)

# vaata andmestikku
str(tekstid)


# Ülesanne 1: tuvasta stringi esinemine
esineb <- str_detect(tekstid$tekst,  "(Eesti)|(eesti)")

# Ülesanne 2: sagedustabeli leidmine
sagedustabel <- table(tekstid[esineb][, 3])
sagedustabel

# Ülesanne 3: osakaalude leidmine
tinglikjaotus <- sagedustabel / table(tekstid[, 3])
tinglikjaotus
#I dont know what I was supposed to do here really

###% Lahendus: 9.1 Ülesanded-----------------------------------------

library(nlme)
apelsinid <- as.data.frame(Orange)

#as.numeric(as.Date("1968-12-31"))

# Vaata andmestikku
head(apelsinid)


# Ülesanne 1: Lisa kuupäeva tunnus andmestikku
apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31")


# Ülesanne 2: Moodusta unikaalsete väärtuste vektor
ajad <- unique(apelsinid$kuupaev)


# Ülesanne 3: Kui pikk on mõõtmistevaheline aeg nädalates?
nadalad <- difftime(ajad[2:7], ajad[1:6],units = "weeks")
nadalad

###% Lahendus: 10.1 Ülesanded-----------------------------------------

haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))

# vaata andmestikku
str(haigla)

library(dplyr)


# Ülesanne 1: leia enne haiglasse tulekut lahkunud isikud
vead1 <- filter(haigla, haigla$haiglast.kp-haigla$haiglasse.kp < 0)
vead1



# Ülesanne 2: leia need, kelle haiglast lahkumise kuupäev pole teada
vead2 <- filter(haigla, is.na(haigla$haiglast.kp-haigla$haiglasse.kp))
vead2

















