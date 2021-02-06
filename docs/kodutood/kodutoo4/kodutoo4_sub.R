###% --------- R kodutöö nr. 4 ---------


# ----- 1. Tulpdiagamm esinemissagedustega -----

# Jootraha andmestik paketist reshape2:

library(reshape2)

jootraha <- tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))

# Tutvu andmetega:

summary(jootraha)


# 1.1 ÜLESANDED

# Ülesande 1 lahendus

library(ggplot2)

# Ülesande 2 lahendus

j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill = "royalblue")  + 
  scale_x_discrete(name = "Day of week", 
                   limits = c("Thur", "Fri", "Sat", "Sun"),
                   labels = c("thursday", "friday", "saturday", "sunday"))  +
  ylab(label = "Counts")
j1


###% ----- 2. Tulpdiagamm osakaaludega -----

# Vaata tunnuste nimed üle (nädalapäev: day, söögiaeg: time):

names(jootraha)


# 2.1 ÜLESANDED

# Ülesande 1 lahendus

library(scales)

j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
  geom_bar(position = "fill")  +
  scale_y_discrete(name = "Percentage",
                   limits = c(0, 1),
                   labels = c("0%", "100%")) + 
  scale_fill_hue(name = "Time") +
  xlab(label = "Day")
j1


###% ----- 3. Hajuvusdiagramm 1 -----

# Tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, day nädalapäev):

names(jootraha)


# 3.1 ÜLESANDED

# Ülesande 1 lahendus

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = day)) +
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))

j1

# Ülesande 2 lahendus

suurim  <- "Sat"


###% ----- 4. Hajuvusdiagramm 2 -----

# Tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, sex sugu, smoker suitsetaja olemasolu):

names(jootraha)


# 4.1 ÜLESANDED

# Ülesande 1 lahendus

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = sex, shape = smoker)) 
j1

# Ülesande 2 lahendus

j1 + geom_smooth(method = lm)


###% ----- 5. Histogramm jaotuse iseloomustamiseks -----

# Tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites):

names(jootraha)


# 5.1 ÜLESANDED

# Ülesande 1 lahendus

jootraha$ratio <- jootraha$tip/jootraha$total_bill

# Ülesande 2 lahendus

ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(bins = 15)

# Ülesande 3 lahendus

erind <- "Dinner"


###% ----- 6. Histogrammi värvi muutmine -----

ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))


# 6.1 ÜLESANDED

# Ülesande 1 lahendus

# Õige vastuse number kommentaarina: 3


###% ----- 7. Karpdiagrammid -----

# Tunnuste nimed andmestikus (size - laudkonna suurus):

names(jootraha)


# 7.1 ÜLESANDED

# Ülesande 1 lahendus

j1 <- ggplot(jootraha, aes(x = size, y = ratio, group = size)) + 
  geom_boxplot()
j1

# Ülesande 2 lahendus

j1 + labs(x = "laudkonna suurus", 
          y = "jootraha ja arve suhe")

# Ülesande 3 lahendus

tabel <- table(jootraha$size)
tabel


###% ----- 8. Joondiagramm 1 -----

# Tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites, size laudkonna suurus):

names(jootraha)


# 8.1 ÜLESANDED

# Ülesande 1 lahendus

jootraha$tip.per.person <- jootraha$tip/jootraha$size

# Ülesande 2 lahendus

ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)


###% ----- 9. Joondiagramm 2 -----

# Tunnuste nimed andmestikus:

names(jootraha)


# 9.1 ÜLESANDED

# Ülesande 1 lahendus

ggplot(jootraha, aes(x = size, y = tip.per.person, color = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)


###% ----- 10. Joondiagramm 3 -----

# Tunnuste nimed andmestikus:

names(jootraha)


# 10.1 ÜLESANDED

# Ülesande 1 lahendus

ggplot(jootraha, aes(x = size, y = tip.per.person, linetype = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)
