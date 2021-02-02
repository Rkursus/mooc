###% Ülesanne 1.1 

A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
head(A)
# 1.
install.packages("dplyr")
library(dplyr)

# 2.
A1 <- mutate(A, 
             kmi = kaal/(kasv*0.01)^2,
             kaalugrupp = ifelse(kmi<=25, "ala voi normkaal", "ylekaal"))

# 3.
str(A1)


###% Ülesanne 2.1 
str(A1)

# 1.
tabel <- A1 %>%   
  group_by(sugu, elukoht) %>% 
  summarise(n = n(), kesk.vanus = mean(vanus), kesk.kmi = mean(kmi), visiit.osak = sum(visiit==TRUE)/n*100)
tabel

# 2.
# sugu = 1, elukoht = 0


###% Ülesanne 3.1 
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)

names(B)
dim(B)

# 1.
library(stringr) 
B1 <- B %>% 
  select(starts_with("test"))
B1

# 2.
library(reshape2)
tabel <- B1 %>% 
  melt(measure.vars =1:ncol(B1))  %>%  
  group_by(variable)  %>%  
  summarise_all(.funs = c("mean", "sd", "min", "max"))
arrange(tabel, variable)


###% Ülesanne 4.1
antropo <- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/antropo.txt", header=T, sep="\t")
mass<- read.table("https://github.com/Rkursus/sygis2019/raw/master/data/mass.txt", header=T, sep="\t")

str(mass)
str(antropo)
# 1.
mass_char <-  mass %>% mutate_if(.predicate = is.factor, .funs = as.character)
str(mass_char)

# 2.
uus_funktsioon <- function(x) x/10
antropo_cm_kg <- antropo  %>%  mutate_at(.vars = vars(-SEX), .funs = uus_funktsioon)
antropo_cm_kg



###% Ülesanne 5.1
A <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/A.csv", nrows = 45)
B <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/B.csv", nrows = 160)
B <- B[, c("id", "grupp", sort(names(B)[-(1:2)]))]

str(A)
str(B)

# 1.
A1 <- A %>% mutate_if(.predicate = is.factor, .funs = as.character)
B1 <- B %>% mutate_if(.predicate = is.factor, .funs = as.character)

# 2.
AB1 <- B1 %>% semi_join(A1, by = "id")

# 3.
AB2 <- A1 %>% inner_join(B1, by= "id")



###% Ülesanne 6.1
install.packages("data.table")
# 1.
library("data.table")

class(A) #"data.frame"

A <- as.data.table(A)
class(A) #"data.table" "data.frame"

# 2.
tabel1 <- A[vanus > 50 & kaal > 80, .(kmi =  kaal/(kasv*0.01)^2, sirutus)]
tabel1

# 3.
tabel2 <- A[visiit != TRUE, .(kesk.vanus = mean(vanus), kesk.pikkus = mean(kasv)), .(sugu, elukoht)]
tabel2
# kontrollimiseks: A[sugu==1 & elukoht==0 & visiit != T,]


###% Ülesanne 7.1
install.packages('curl')
library(curl)
tekstid <- fread("https://github.com/Rkursus/sygis2019/raw/master/data/tekstid.csv", nrow = 1000, encoding = "UTF-8", 
                 col.names = c("rubriik", "loigunr", "hinnang", "tekst"), select = c(1,3:5))
tekstid <- tekstid[loigunr != "None", ] # Eemaldame read, kus 'loigunr' puudub.

# 1.
is.data.table(tekstid)

# 2.
tekstid[, loigunr := as.integer(loigunr)]

# 3.
valik <- tekstid[loigunr > 2 & startsWith(tekst, "A") ,]


###% Ülesanne 8.1
library(stringr)
str(tekstid)

# 1.
esineb <- str_detect(tekstid$tekst, "[Ee]esti")

# 2.
sagedustabel <- table(tekstid$hinnang, esineb)
sagedustabel

# 3.
tinglikjaotus <- prop.table(sagedustabel, margin = 2)
tinglikjaotus


###% Ülesanne 9.1

library(nlme)
apelsinid <- as.data.frame(Orange)

head(apelsinid)

# 1.
apelsinid$kuupaev <- as.Date(apelsinid$age, origin = "1968-12-31")
apelsinid

# 2.
ajad <- unique(apelsinid$kuupaev)
ajad

# 3.
nadalad <- difftime(ajad[-1], ajad[-length(ajad)], units = "weeks")
nadalad


###% Ülesanne 10.1
haigla <- read.csv2("https://github.com/Rkursus/sygis2019/raw/master/data/haigla.csv", colClasses = c("integer", "Date", "Date"))
str(haigla)

# 1.
#jätsin NA väärtused välja, et näeks ainult juhte, kus lahkumise kuupäev varasem kui saabumise
vead1 <- haigla[difftime(haigla$haiglast.kp, haigla$haiglasse.kp) < 0 & is.na(difftime(haigla$haiglast.kp, haigla$haiglasse.kp)) ==F, ]
vead1

# 2.
vead2 <- haigla[is.na(haigla$haiglast.kp)==T ,]
vead2
