###% --------- R kodutöö nr. 5 ---------


# ----- 1. Pakett dplyr - Andmestikku tunnuste lisamine -----

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)


# 1.1 ÜLESANDED

# Vaata andmestik üle
head(A)


# Ülesanne 1: aktiveeri pakett
_____________


# Ülesanne 2: lisa tunnused
A1 <- mutate(___________________)


# Ülesanne 3: vaata tulemust
____________




###% ----- 2. Pakett dplyr - Grupikokkuvõtete arvutamine -----


# 2.1 ÜLESANDED

# Vaata andmestik üle
str(A1)


# Ülesanne 1: leia tabel
tabel <- A1 %>%   
  ______________ %>% 
  ____________________________
tabel

# Ülesanne 2: leia mis grupp on kõige madalama arstivisiidil käimise osakaaluga.
sugu <- ___
elukoht <- ___


###% ----- 3. Pakett dplyr - Mitmele tunnusele kirjeldavate karakteristikute leidmine -----

B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)

# 3.1 ÜLESANDED

# Vaata üle tunnusenimed ja andmestiku dimensioonid
names(B)
dim(B)

# Ülesanne 1: Alamandmestiku valik
B1 <- __________________________________



# Ülesanne 2: Täienda koodi
library(____________)
tabel <- B1 %>% 
  __________________  %>%  
  _____________(___________)  %>%  
  ______________(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)


###% ----- 4. Pakett dplyr - Mitmele tunnusele teisenduse määramine -----

antropo <- read.table("https://github.com/Rkursus/mooc/raw/main/data/antropo.txt", header = T, sep = "\t")
mass<- read.table("https://github.com/Rkursus/mooc/raw/main/data/mass.txt", header = T, sep = "\t")


# 4.1 ÜLESANDED

# Tutvu andmestikega
str(mass)
str(antropo)

# Ülesanne 1: teisenda faktorid tavaliseks tekstiks
_________ <-  __________ %>% mutate______(.predicate = __________, .funs = __________)


# Ülesanne 2: teisenda ühikud
uus_funktsioon <- function(_____) ____________
antropo_cm_kg <- _______  ____  mutate______(.vars = vars(-SEX), .funs = uus_funktsioon)





###% ----- 5. Pakett dplyr - Andmestike ühendamine -----

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)
B <- B[ , c("id", "grupp", sort(names(B)[-(1:2)]))]


# 5.1 ÜLESANDED

# Vaata anmdestikud üle
str(A)
str(B)


# Ülesanne 1:  Teisenda tunnuse tüüp
A1 <- A %>% mutate_if(__________)
B1 <- B %>% mutate_if(__________)


# Ülesanne 2: ühenda andmestikud
AB1 <- ___ %>% _________________

# Ülesanne 3: ühenda andmestikud
AB2 <- ___ %>% _________________





###% ----- 6. Pakett data.table - Ridade filtreerimine ja veergude valik/defineerimine -----


# 6.1 ÜLESANDED

# Ülesanne 1: aktiveeri pakett
_______________________
class(A) # Mis tüüpi on andmestik A?

A <- as._________(A)
class(A) # Mis tüüpi nüüd?

# Ülesanne 2: tee valik objektidest ja vali/arvuta tunnused
tabel1 <- A[______, ______]
______

# Ülesanne 3: tee valik objektidest ja vali/arvuta tunnused gruppide kaupa
tabel2 <- A[______, ______, ______]
______




###% ----- 7. Pakett data.table - Tunnuse tüübi teisendus, sagedustabeli leidmine -----

tekstid <- fread("https://github.com/Rkursus/mooc/raw/main/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] # eemaldame read, kus 'loigunr' puudub.


# 7.1 ÜLESANDED

# Ülesanne 1: kontrolli, kas andmestik on data.table-tüüpi
_______________________

# Ülesanne 2: tee tüübiteisendus
tekstid[_________]


# Ülesanne 3: vali read ja leia sagedused (ära pane veergudele uusi nimesid)
valik <- tekstid[___________]





###% ----- 8. Töö tekstiga - Teksti esinemise kontroll -----


# 8.1 ÜLESANDED

library(stringr)

# vaata andmestikku
str(tekstid)


# Ülesanne 1: tuvasta stringi esinemine
esineb <- _______________________

# Ülesanne 2: sagedustabeli leidmine
sagedustabel <- table(______________)
sagedustabel

# Ülesanne 3: osakaalude leidmine
tinglikjaotus <- __________________
tinglikjaotus





###% ----- 9. Töö kuupäevadega - Date-tüüpi muutuja loomine -----

library(nlme)

apelsinid <- as.data.frame(Orange)


# 9.1 ÜLESANDED

# Vaata andmestikku
head(apelsinid)


# Ülesanne 1: Lisa kuupäeva tunnus andmestikku
apelsinid$kuupaev <- _______________________


# Ülesanne 2: Moodusta unikaalsete väärtuste vektor
ajad <- ______(apelsinid$kuupaev)


# Ülesanne 3: Kui pikk on mõõtmistevaheline aeg nädalates?
nadalad <- difftime(___________)
nadalad




###% ----- 10. Töö kuupäevadega - Kuupäevade võrdlemine -----

haigla <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/haigla.csv", colClasses = c("integer", "Date", "Date"))

# Vaata andmestikku:
str(haigla)


# 10.1 ÜLESANDED

# vaata andmestikku
str(andmed)


# Ülesanne 1: leia enne haiglasse tulekut lahkunud isikud
vead1 <- _________________________________
vead1



# Ülesanne 2: leia need, kelle haiglast lahkumise kuupäev pole teada
vead2 <- _________________________________
vead2
