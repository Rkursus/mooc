###% --------- R kodutöö nr. 4 ---------


# ----- 1. Tulpdiagamm esinemissagedustega -----

# Jootraha andmestik paketist reshape2:

library(reshape2)

jootraha <- tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))


# 1.1 ÜLESANDED

# Tutvu andmetega
summary(jootraha)


# Ülesanne 1: aktiveeri pakett
_______(ggplot2)

# Ülesanne 2: täienda koodi
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom________  + 
  scale_x____________   +
  ylab(label = "_________")
j1

###% ----- 2. Tulpdiagamm osakaaludega -----


# 2.1 ÜLESANDED

# Vaata tunnuste nimed üle (nädalapäev: day, söögiaeg: time)
names(jootraha)

# Ülesanne : tulpdiagramm osakaaludega
library(scales)
j1 <- ggplot(jootraha, aes(x = ________, ________ = time)) + 
  geom___________  +
  scale______________ + 
  scale______________ +
  xlab_______ 
j1


###% ----- 3. Hajuvusdiagramm 1 -----


# 3.1 ÜLESANDED

# tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, day nädalapäev)
names(jootraha)

# Ülesanne1: Hajuvusdiagramm värviliste punktidega
j1 <- ggplot(jootraha, aes(x = ______, y = _______)) + 
  geom____________  +
  scale_______hue(________________)
j1


# Ülesanne 2: Suurima arvega päev
suurim  <- "_____"




###% ----- 4. Hajuvusdiagramm 2 -----


# 4.1 ÜLESANDED

# tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, sex sugu, smoker suitsetaja olemasolu)
names(jootraha)

# Ülesanne 1: Hajuvusdiagramm värviliste ja eri kujuga punktidega
j1 <- ggplot(jootraha, aes(x = ______, y = _______)) + 
  geom____________  
j1


# Ülesanne 2:  Regressioonsirge lisamine
j1 + ________________________________





###% ----- 5. Histogramm jaotuse iseloomustamiseks -----


# 5.1 ÜLESANDED

# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites)
names(jootraha)

# Ülesanne 1: Uue tunnuse moodustamine
jootraha$_____ <- _______________


# Ülesanne 2:  Histogramm (ära muuda 'y = ..density..' osa koodist!)
ggplot(jootraha, aes(__ = __________, y = ..density..)) + geom_________


# Ülesanne 3: Kas erandlik laudkond käis õhtu või lõunasöögil?
erind <- "_______"




###% ----- 6. Histogrammi värvi muutmine -----

# Lisame histogrammi joonistamise käsku värvi järgmisel moel:

ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))

# 6.1 ÜLESANDED

# Ülesanne 1
# Õige vastuse number
vastus <- ____


###% ----- 7. Karpdiagrammid -----


# 7.1 ÜLESANDED

# tunnuste nimed andmestikus (size - laudkonna suurus)
names(jootraha)

# Ülesanne 1: Karpdiagramm
j1 <- ggplot(jootraha, aes(x = _______, y = _______, group = _______)) + geom_________
j1

# Ülesanne 2: Täienda joonist teljenimedega
j1 + labs(________, ___________)

# Ülesanne 3: Sagedustabel
tabel <- ________________
tabel




###% ----- 8. Joondiagramm 1 -----


# 8.1 ÜLESANDED

# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites, size laudkonna suurus)
names(jootraha)

# Ülesanne 1: Uue tunnuse moodustamine
jootraha$_____ <- _______________


# Ülesanne 2: Joondiagramm
ggplot(jootraha, aes(x = _______, y = ____________)) + 
  stat_summary(geom = "_______",  fun = _________)  + 
  scale_x_continuous(breaks = 1:6)




###% ----- 9. Joondiagramm 2 -----


# 9.1 ÜLESANDED

# tunnuste nimed andmestikus 
names(jootraha)


# Ülesanne : Joondiagramm (soo kaupa)
ggplot(jootraha, aes(x = _______, y = ____________, ________________)) + 
  stat_summary(geom = "_______",  fun = _________)  + 
  scale_x_continuous(breaks = 1:6)




###% ----- 10. Joondiagramm 3 -----


# 10.1 ÜLESANDED

# tunnuste nimed andmestikus 
names(jootraha)


# Ülesanne : Joondiagramm (soo kaupa)
ggplot(jootraha, aes(x = _______, y = ____________, ________________)) + 
  stat_summary(geom = "_______",  fun = _________)  + 
  scale_x_continuous(breaks = 1:6)


