###%Ülesanne 1 lahendus
library(reshape2)
jootraha <-tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))

#Ülesanne 1.1 lahendus
#1
install.packages("ggplot2")
library(ggplot2)

#2
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(aes(fill = "royalblue"))  + 
  scale_x_discrete(name="Day of week", limits = c("Thu", "Fri", "Sat", "Sun"), labels = c("thursday", "friday", "saturday", "sunday"))   +
  ylab(label = "Counts")
j1

###%Ülesanne 2.1 lahendus
names(jootraha)
library(scales)
j1 <- ggplot(jootraha, aes(x = day, position = time)) + 
  geom_bar(aes(fill=time), position="fill")  +
  scale_colour_hue(labels("Time")) + 
  scale(labels("Percentage")) +
  xlab("Day") 
j1

###%Ülesanne 3.1 lahendus

names(jootraha)
#1
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = day))  +
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
j1

#2
suurim  <- "Sat"

###%Ülesanne 4.1 lahendus
names(jootraha)

#1
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color= sex, shape=smoker))
j1

#2
j1 + geom_smooth(method = lm)

###%Ülesanne 5.1 lahendus
names(jootraha)

#1
jootraha$ratio <- jootraha$tip/ jootraha$total_bill


#2
ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram()

#3
erind <- "Dinner"

###%Ülesanne 6.1 lahendus
ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))
#1 
#ei
#2
#ei
#3
#jah
#4
#ei
#5
#ei
#6
#jah

###%Ülesanne 7.1 lahendus
names(jootraha)

#1
j1 <- ggplot(jootraha, aes(x = tip, y = total_bill, group = size)) + 
  geom_boxplot()
j1

#2
j1 + labs(x="laudkonna suurus", y="jootraha ja arve suhe")

#3
tabel <- table(jootraha, aes(x = tip, y = total_bill,group = size))
tabel

###%Ülesanne 8.1 lahendus

names(jootraha)

#1
jootraha$tip.per.person <- jootraha$tip / jootraha$size


#2
ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 9.1 lahendus
names(jootraha)
ggplot(jootraha, aes(x = size, y = tip.per.person, color=sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 10.1 lahendus
ggplot(jootraha, aes(x = size, y =tip.per.person, linetype=sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

