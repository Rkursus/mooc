###% --------- R kodutöö nr. 5 ---------


# ----- 1. Pakett dplyr - Andmestikku tunnuste lisamine -----

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)

# Vaata andmestik üle:

head(A)


# 1.1 ÜLESANDED

# Ülesande 1 lahendus

library(dplyr)

# Ülesande 2 lahendus

A1 <- mutate(A,
             kmi = kaal/(kasv/100)^2,
             kaalugrupp = ifelse(kmi <= 25, "ala voi normkaal", "ylekaal"))

# Ülesande 3 lahendus

str(A1)


###% ----- 2. Pakett dplyr - Grupikokkuvõtete arvutamine -----

# Vaata andmestik üle:

str(A1)


# 2.1 ÜLESANDED

# Ülesande 1 lahendus

tabel <- A1 %>%   
  group_by(sugu, elukoht) %>% 
  summarise(n = n(),
            kesk.vanus = mean(vanus),
            kesk.kmi = mean(kmi),
            visiit.osak = table(visiit)[2]/n)
tabel


# Ülesande 2 lahendus

# sugu = 1, elukoht = 0


###% ----- 3. Pakett dplyr - Mitmele tunnusele kirjeldavate karakteristikute leidmine -----

B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)

# Vaata üle tunnusenimed ja andmestiku dimensioonid:

names(B)
dim(B)


# 3.1 ÜLESANDED

# Ülesande 1 lahendus

B1 <- B %>%
  select(starts_with("test"))

# Ülesande 2 lahendus

library(reshape2)

tabel <- B1 %>% 
  melt() %>%  
  group_by(variable) %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)


###% ----- 4. Pakett dplyr - Mitmele tunnusele teisenduse määramine -----

antropo <- read.table("https://github.com/Rkursus/mooc/raw/main/data/antropo.txt", header = T, sep = "\t")
mass<- read.table("https://github.com/Rkursus/mooc/raw/main/data/mass.txt", header = T, sep = "\t")

# Tutvu andmestikega:

str(mass)
str(antropo)


# 4.1 ÜLESANDED

# Ülesande 1 lahendus

mass_char <- mass %>%
  mutate_if(.predicate = is.factor, .funs = as.character)
str(mass_char)

# Ülesande 2 lahendus

uus_funktsioon <- function(x) x/10
antropo_cm_kg <- antropo %>%
  mutate_at(.vars = vars(-SEX), .funs = uus_funktsioon)


###% ----- 5. Pakett dplyr - Andmestike ühendamine -----

A <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/mooc/raw/main/data/B.csv", nrows = 160)
B <- B[ , c("id", "grupp", sort(names(B)[-(1:2)]))]

# Vaata anmdestikud üle:

str(A)
str(B)


# 5.1 ÜLESANDED

# Ülesande 1 lahendus

A1 <- A %>% 
  mutate_if(.predicate = is.factor, .funs = as.character)

B1 <- B %>% 
  mutate_if(.predicate = is.factor, .funs = as.character)

str(A1)
str(B1)

# Ülesande 2 lahendus

AB1 <- B1 %>% 
  semi_join(A1, by = "id")

# Ülesande 3 lahendus

AB2 <- A1 %>%
  inner_join(B1, by = "id")


###% ----- 6. Pakett data.table - Ridade filtreerimine ja veergude valik/defineerimine -----


# 6.1 ÜLESANDED

# Ülesande 1 lahendus

library(data.table)

class(A) # Mis tüüpi on andmestik A?
A <- as.data.table(A)
class(A) # Mis tüüpi nüüd? data.table ja data.frame

# Ülesande 2 lahendus

tabel1 <- A[vanus > 50 & kaal > 80, 
            .(kmi = kaal/(kasv/100)^2, sirutus)]
tabel1

# Ülesande 3 lahendus

tabel2 <- A[visiit == FALSE, 
            .(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv)), 
            by = list(sugu, elukoht)]
tabel2


###% ----- 7. Pakett data.table - Tunnuse tüübi teisendus, sagedustabeli leidmine -----

tekstid <- fread("https://github.com/Rkursus/mooc/raw/main/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] # eemaldame read, kus 'loigunr' puudub.


# 7.1 ÜLESANDED

# Ülesande 1 lahendus

is.data.table(tekstid)

# Ülesande 2 lahendus

is.integer(tekstid$loigunr)

tekstid[ , loigunr := as.integer(loigunr)]

is.integer(tekstid$loigunr)

# Ülesande 3 lahendus

valik <- tekstid[loigunr > 2 & startsWith(tekst, "A"), .(n = .N), by = hinnang]


###% ----- 8. Töö tekstiga - Teksti esinemise kontroll -----

library(stringr)

# vaata andmestikku:

str(tekstid)


# 8.1 ÜLESANDED

# Ülesande 1 lahendus

esineb <- str_detect(tekstid$tekst, pattern = "[Ee]sti")

# Ülesande 2 lahendus

sagedustabel <- table(tekstid$hinnang, esineb)
sagedustabel

# Ülesande 3 lahendus

tinglikjaotus <- prop.table(sagedustabel, margin = 1)
tinglikjaotus


###% ----- 9. Töö kuupäevadega - Date-tüüpi muutuja loomine -----

library(nlme)

apelsinid <- as.data.frame(Orange)

# Vaata andmestikku:

head(apelsinid)


# 9.1 ÜLESANDED

# Ülesande 1 lahendus

apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31") 

# Ülesande 2 lahendus

ajad <- unique(apelsinid$kuupaev)
ajad

# Ülesande 3 lahendus

nadalad <- difftime(ajad, shift(ajad, n = 1, ajad[length(ajad)]), units = "weeks")
nadalad


###% ----- 10. Töö kuupäevadega - Kuupäevade võrdlemine -----

haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/main/data/haigla.csv", colClasses = c("integer", "Date", "Date"))

# Vaata andmestikku:
str(haigla)


# 10.1 ÜLESANDED

# Ülesande 1 lahendus

vead1 <- haigla[!is.na(haigla$haiglasse.kp) & 
                  !is.na(haigla$haiglast.kp) & 
                  haigla$haiglasse.kp > haigla$haiglast.kp, ]
vead1

# Ülesande 2 lahendus

vead2 <- haigla[is.na(haigla$haiglast.kp), ]
vead2
