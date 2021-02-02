###% Lahendus: 1.1 Ülesanded-----------------------------------------

# jootraha andmestik paketist reshape2
#install.packages("reshape2")
library(reshape2)
jootraha <- tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))

# Tutvu andmetega
summary(jootraha)


# Ülesanne 1: aktiveeri pakett
#install.packages("ggplot2")
library(ggplot2)

# Ülesanne 2: täienda koodi
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill = "royalblue")  + 
  scale_x_discrete(name = "Day of week", 
                   labels = c("Thur" = "thursday", "Fri" = "friday", "Sat" = "saturday", "Sun" = "sunday"),
                   limits = c("Thur", "Fri", "Sat", "Sun") #order and what to include
                   )  +
  ylab(label = "Counts")
j1


###% Lahendus: 2.1 Ülesanded-----------------------------------------


# Vaata tunnuste nimed üle (nädalapäev: day, söögiaeg: time)
names(jootraha)

# Ülesanne : tulpdiagramm osakaaludega
library(scales)
j1 <- ggplot(jootraha, aes(x = day, fill = time)) + #time is a style, specify what on the x-axis w/ x =
  geom_bar(position = "fill") + #to make the height always add up to 100%
  scale_y_continuous(name = "Percentage", #change to continous and not discrete
                   labels = percent) +
  scale_fill_hue(name = "Time") + #change title of legend
  xlab(label = "Day")
j1

###% Lahendus: 3.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, day nädalapäev)
names(jootraha)

# Ülesanne1: Hajuvusdiagramm värviliste punktidega
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = day))  + #add color of dots according to day
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun")) # change order of dots in legend
#this is the 2nd time we are supposed to change the order of something, but in both cases it has already been in a
#fitting order to begin with, I would say because we did in the beginning the factor command. So i dont understand
#why would need to use the limits command
j1

# Ülesanne 2: Suurima arvega päev
suurim  <- "Sat"

###% Lahendus: 4.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, sex sugu, smoker suitsetaja olemasolu)
names(jootraha)

# Ülesanne 1: Hajuvusdiagramm värviliste ja eri kujuga punktidega
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = sex, shape = smoker)) # aes() for changes to points inside
j1


# Ülesanne 2:  Regressioonsirge lisamine
j1 + geom_smooth(method = lm)

###% Lahendus: 5.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites)
names(jootraha)

# Ülesanne 1: Uue tunnuse moodustamine
jootraha$ratio <- jootraha$tip/jootraha$total_bill


# Ülesanne 2:  Histogramm (ära muuda 'y = ..density..' osa koodist!)
ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins = 15)


# Ülesanne 3: Kas erandlik laudkond käis õhtu või lõunasöögil?
#jootraha[jootraha$ratio > 0.5, ]
erind <- "Dinner"

###% Lahendus: 6.1 Ülesanded-----------------------------------------

ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))

# Ülesanne 1
# 3. Tulemuseks oleval joonisel on histogrammi tulbad punast tooni.
# Mõtle kas andmestikus on tunnus, mille nimi on chartreuse?
# It doesnt, you could even give an empty string fill = ""

###% Lahendus: 7.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus (size - laudkonna suurus)
names(jootraha)

# Ülesanne 1: Karpdiagramm
j1 <- ggplot(jootraha, aes(x = size, y = ratio, group = size)) + geom_boxplot()
j1

# Ülesanne 2: Täienda joonist teljenimedega
j1 + labs(y = "jootraha ja arve suhe", x = "laudkonna suurus")

# Ülesanne 3: Sagedustabel
tabel <- table(jootraha$size)
tabel

###% Lahendus: 8.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites, size laudkonna suurus)
names(jootraha)

# Ülesanne 1: Uue tunnuse moodustamine
jootraha$tip.per.person <- jootraha$tip/jootraha$size


# Ülesanne 2: Joondiagramm
ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###% Lahendus: 9.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus 
names(jootraha)


# Ülesanne : Joondiagramm (soo kaupa)
ggplot(jootraha, aes(x = size, y = tip.per.person, color = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###% Lahendus: 10.1 Ülesanded-----------------------------------------

# tunnuste nimed andmestikus 
names(jootraha)


# Ülesanne : Joondiagramm (soo kaupa)
ggplot(jootraha, aes(x = size, y = tip.per.person, linetype = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)




