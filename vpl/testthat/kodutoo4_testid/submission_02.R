###% Ülesanne 1.1 lahendus
library(reshape2)
jootraha <-tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))
summary(jootraha)

library(ggplot2)

j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill = 'royalblue')+ 
  scale_x_discrete(name = "Day of week", limits = c("Thur", "Fri", "Sat", "Sun"),
                   labels = c("thursday", "friday", "saturday", "sunday"))   +
  ylab(label = "Counts")
j1

###%Ülesanne 2.1 lahendus
names(jootraha)

library(scales)
j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
  geom_bar(position="fill")  +
  scale_y_continuous(labels = percent, name = "Percentage") + 
  scale_colour_hue(name = "Time") +
  xlab(label = "Day") 
j1

###%Ülesanne 3.1 lahendus
names(jootraha)

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = day))  +
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
j1

suurim  <- "Sat"

###%Ülesanne 4.1 lahendus
names(jootraha)

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = sex, shape = smoker))  
j1

j1 + geom_smooth(method = lm)

###%Ülesanne 5.1 lahendus
names(jootraha)

jootraha$ratio <- jootraha$tip/jootraha$total_bill

ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins = 15)

erind <- "Dinner"

###%Ülesanne 6.1 lahendus
#Õige vastus on 3.

###%Ülesanne 7.1 lahendus
names(jootraha)

j1 <- ggplot(jootraha, aes(x = size, y = ratio, group = size)) + geom_boxplot()
j1

j1 + labs(x = "laudkonna suurus", y = "jootraha ja arve suhe")

tabel <- table(jootraha$size)
tabel

###%Ülesanne 8.1 lahendus
names(jootraha)

jootraha$tip.per.person <- jootraha$tip/jootraha$size

ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 9.1 lahendus
ggplot(jootraha, aes(x = size, y = tip.per.person, colour = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 10.1 lahendus
ggplot(jootraha, aes(x = size, y = tip.per.person, linetype = sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)
