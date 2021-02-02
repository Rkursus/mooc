###% Ülesanne 1.1.1

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)

head(A)

library(dplyr)

# Ülesanne 1.1.2

A1 <- mutate(A, kmi = (kaal / kasv)^2*100, ala_voi_normkaal = ("kmi"<= 25), ylekaal = ("kmi" > 25), 
             ifelse(is.na(A$kaalugrupp), ala_voi_normkaal, ylekaal))

# Ülesanne 1.1.3

str(A1)

###% Ülesanne 2.1.1

tabel <- A1 %>% group_by(sugu, elukoht) %>% 
  summarise(kesk.vanus = mean(vanus), kesk.kmi = mean(kmi), visiit.osakaal = mean(visiit), n = n())

tabel

# Ülesanne 2.1.2

# sugu = 1, elukoht = 0


###%  Ülesanne 3.1.1

B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)

names(B)
dim(B)

B1 <- B %>% select(starts_with("test")) 


# Ülesanne 3.1.2 

library(reshape2)
install.packages("reshape2")

tabel <- B1 %>% melt() %>% group_by(starts_with("test")) %>% 
              summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)

###%  Ülesanne 4.1.1

antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")

str(mass)
str(antropo)

mass_char <- mass %>% mutate_if(is.factor, as.character)

# Ülesanne 4.1.2 

uus_funktsioon <- function(x) x/10
antropo_cm_kg <-     %>% mutate_all(.vars = vars(-SEX), .funs = uus_funktsioon)


###% Ülesanne 5.1.1
  
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

str(A)
str(B)

A1 <- A %>% mutate_if(is.factor, as.character)
B1 <- B %>% mutate_if(is.factor, as.character)

# Ülesanne 5.1.2

AB1 <- semi_join(B, A, by = "id")

# Ülesanne 5.1.3

AB2 <- inner_join(A, B, by = "id")


###%  Ülesanne 6.1.1

library(data.table)
install.packages("data.table")

class(A)  # andmestik on data.frame tüüpi

A <- as.data.table(A)
class(A)  # andmestik on data.table tüüpi


# Ülesanne 6.1.2

tabel1 <- A[vanus > 50, kaal > 80, list(kmi = (kaal/kasv)**2, sirutus)]
tabel1

###% Ülesanne 6.1.3

tabel2 <- A[sugu, elukoht, 
            list(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv), visiit = FALSE)]
tabel2

###% Ülesanne 7.1.1

tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ]

is.data.table(tekstid)

# Ülesanne 7.1.2

tekstid[, loigunr := as.numeric(loigunr)]

# Ülesanne 7.1.3

valik <- tekstid[loigunr > 2, startsWith(A), by = "hinnang"]


###% Ülesanne 8.1.1

library(stringr)
str(tekstid)

esineb <- str_detect("tekst", "[[Ee]sti]")

# Ülesanne 8.1.2

sagedustabel <- table(tekstid$hinnang, tekstid$esineb)
sagedustabel

# Ülesanne 8.1.3

tinglikjaotus <- prop.table(sagedustabel)
tinglikjaotus


###% Ülesanne 9.1.1

library(nlme)
apelsinid <- as.data.frame(Orange)

head(apelsinid)

apelsinid$kuupaev <- as.Date("1968-12-31", "%Y-%m-%d")

# Ülesanne 9.1.2

ajad <- unique(c(118, 484, 664, 1004, 1231, 1372, 1582), apelsinid$kuupaev)

# Ülesanne 9.1.3 

nadalad <- difftime(time1 = c(118, 484, 664, 1004, 1231, 1372, 1582), 
                    time2 = c(484, 664, 1004, 1231, 1372, 1582), units = c("weeks"))
nadalad

###% Ülesanne 10.1.1

haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", 
                    colClasses = c("integer", "Date", "Date"))

str(haigla)

vead1 <- as.numeric(haigla$haiglast.kp - haigla$haiglasse.kp)
vead1

# Ülesanne 10.1.2

vead2 <- haigla[is.na(haigla$haiglast.kp), ]
vead2

  