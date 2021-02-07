###% --------- R kodutöö nr. 3 ---------


# ----- 1. Faktor-tüüpi tunnuse kasutamine -----

# ---- 1.1 Faktori tasemete järjestamine ----


# 1.1.1 ÜLESANDED

# Vaata esmalt andmestikust ülevaadet
summary(iris)


# Ülesanne 1: Leia keskmised kroonlehepikkused käsu by abil
keskmised <- by(_______________)
keskmised


# Ülesanne 2: Mis sordil keskmiselt kõige lühemad kroonlehed
kroonlehed1 <- "_________"


# Ülesanne 3: Muuda sordinimede järjestust, tekita selleks uus tunnus
iris$sordinimi <- ____________________________


# Ülesanne 4: Leia maksimaalne kroonlehe pikkus sortide kaupa, sordid olgu uues järjestuses
maksimumid <- tapply(____________)
maksimumid




###% ---- 1.2 Faktor-tunnuse loomine arvtunnusest ----


# 1.2.1 ÜLESANDED

#Ülesanne 1 Jaga kroonlehtede pikkus intervallidesse
intervallid <- cut(______________)


#Ülesanne 2: Kas vektor intervallid on faktor-tüüpi?
__.______(intervallid)


#Ülesanne 3: Sagedustabel
sagedustabel <- ________(intervallid)
sagedustabel


#Ülesanne 4: Mitu tühja intervalli tekkis?
tyhjad <- __





###% ----- 2. Andmestiku sorteerimine -----

# ---- 2.1 Sorteerimine ----

x <- c(2:1, 2:1, 2:1, 4)
y <- c(7, 1, 5, 2, 6, 3, 4)
xy <- data.frame(x, y)


# 2.1.1 ÜLESANDED

# Vaata vektorite x, y ja andmestiku xy sisu
x; y; xy

# Ülesanne 1: andmestiku sorteerimine kasvavalt
xy1 <- xy[_____, ]
xy1


# Ülesanne 2: ridade järjestus order(x, -y) põhjal
xy2 <- xy[_____, ]
xy2


# Ülesanne 3: Milles seisneb erinevus? (vastuseks kirjuta õige väite number)
oige_vaide <- _______






###% ---- 2.2 Andmestiku sorteerimine kasvavalt ----

# Töölaual on andmestik iris. Andmestik tuleb erinevate tunnuste järgi kasvavalt sorteerida.


# 2.2.1 ÜLESANDED

# vaata andmestikku
head(iris)


# Ülesanne 1: sorteerimine
iris.sort1 <- iris[____, ]


# ülesanne 2: eelviimase lille sort
eelviimane <- "_______"




###% ---- 2.3 Andmestiku sorteerimine ----

# Siin ülesandes tuleb kombineerida kasvavalt ja kahanevalt sorteerimist.


# 2.3.1 ÜLESANDED

# vaata andmestikku
head(iris)


# Ülesanne 1: sorteerimine
iris.sort2 <- iris[____, ]


# ülesanne 2: kolmekümnenda lille sort
kolmekymnes <- "_______"


  

###% ----- 3. Andmestiku kuju teisendused -----

# ---- 3.1 Andmestiku viimine pikka formaati ----

B <-  read.csv2(file = "https://github.com/Rkursus/2020/raw/master/data/B.csv",   
                nrows = 160, stringsAsFactors = T)
B <- B[ , substr(names(B), 1, 3) %in% c("id", "gru", "tes") ]


# 3.1.1 ÜLESANDED

# Vaata andmestiku kirjeldust
str(B)


# Ülesanne 1: aktiveeri lisapakett
______(reshape2)


# Ülesanne 2: vii andmestik pikale kujule
testid.pikk <- melt(_______________________)


# Ülesanne 3: vaata andmestiku struktuuri
_____(testid.pikk)


###% ---- 3.2 Andmestiku viimine laia formaati ----

rotid <- read.csv('https://github.com/Rkursus/mooc/raw/main/data/rotid.csv', header = T, stringsAsFactors = F, 
                  colClasses = c("numeric", "character", "character", "factor"))

# Rotid on jagatud 3 gruppi, iga grupp sai erinevat toitu ('Diet'). Sööda mõju uurimiseks kaaluti rotid korduvalt, 
#  mõõtmisajad on kirjas tunnuses 'Time' (aeg päevades katse algusest) kaal grammides on toodud veerus 'weight'


# 3.2.1 ÜLESANDED

# vaata andmestikku
summary(rotid)


# Ülesanne 1: aktiveeri lisapakett
______(reshape2)


# Ülesanne 2: vii andmestik laiale kujule
rotid.lai <- dcast(_______________________)


# Ülesanne 3: prindi uus andmestik ekraanile
____________


###% ---- 3.3 Andmestiku agregeerimine 1 ----

# Kasuta sama andmestikku rotid. Pakett reshape2 peaks olema juba aktiveeritud


# 3.3.1 ÜLESANDED

# vaata andmestikku
summary(rotid)


# Ülesanne 1: Leia sagedustabel
tabel1 <- ________________
tabel1


# ÜLesanne 2: Leia samad näitajad käsu dcast abil
tabel2 <- dcast(_______________________)
tabel2


# Ülesanne 3: Kui palju on katkestajaid
katkestajaid <- _______




###% ---- 3.4 Andmestiku agregeerimine 2 ----

# Kasuta sama andmestikku 'rotid'.

# 3.4.1 ÜLESANDED

# Ülesanne 1: Leia mediaanide tabel dcast käsuga
tabel3 <- ________________
tabel3


# Ülesanne 2: roti nr 2 kaalu mediaan
rott2mediaan <- _______


###% ---- 3.5 Tabeli pööramine ----

# Loe töölauale tabel nimega arstiabita, kus on kirjas eri aastatel arsiabi mittesaanud 
#  inimeste osakaalud (Eesti sotsiaaluuringu andmete põhjal). 

arstiabita <- read.table("https://github.com/Rkursus/mooc/raw/main/data/eisaanud-arstiabi.txt", 
                         sep = "\t", dec = ",", header = T, check.names = FALSE)

# 3.5.1 ÜLESANDED

# Ülesanne 1: prindi andmestik ekraanile
___________


# Ülesanne 2: vii andmestik pikale kujule, vaata tulemust
pikk <- melt(arstiabita, variable.name = ___________________)
______


# Ülesanne 3: teisenda pikk andmestik laiaks nii, et tulemuseks on pööratud andmestik. Prindi tulemus ekraanile
transponeeritud <- dcast(pikk, __________~___________)
___________




###% ---- 3.6 Tabeli pööramine ühe sammuga ----

# Käsk 'recast' võimaldab kombineerida järjestikused pikk -> lai teisendused. Enne ülesande lahendamist vaata käsu abilehte:
?recast


# 3.6.1 ÜLESANDED

# Ülesanne 1: pööra andmestik. Prindi tulemus ekraanile
transponeeritud <- recast(________________)
___________


