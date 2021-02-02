###%Ülesanne 1.1.1 lahendus
library(reshape2)
jootraha <- tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))
summary(jootraha)

install.packages("ggplot2")
library(ggplot2)

#Ülesanne 1.1.2 lahendus
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill="royalblue") + 
  scale_x_discrete(name="Day of the week",
                   limits=c("Thur","Fri","Sat","Sun"),
                   labels=c("Thur"="thursday",
                            "Fri"="friday",
                            "Sat"="saturday",
                            "Sun"="sunday"))   +
  ylab(label = "Counts")
j1

###%Ülesanne 2.1 lahendus
names(jootraha)

library(scales)
j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
  geom_bar(position="fill")  +
  scale_fill_discrete(name="Time")+
  scale_y_continuous(name="Percentage",
                     labels=c("0 %", "25 %", "50 %", "75 %", "100 %")) + 
  xlab(label="Day") 
j1

###%Ülesanne 3.1.1 lahendus
# tunnuste nimed andmestikus(total_bill on arve, tip on jootraha dollarites, day nädalapäev)
names(jootraha)

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = day))  +
  scale_color_hue(limits=c("Thur","Fri","Sat","Sun"))
j1

#Ülesanne 3.1.2 lahendus
suurim <- "Sat ehk laupäev"

###%Ülesanne 4.1.1 lahendus
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = sex, shape=smoker))
j1

#Ülesanne 4.1.2 lahendus
j1 + geom_smooth(method=lm)

###%Ülesanne 5.1.1 lahendus
# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites)
names(jootraha)
jootraha$ratio <- jootraha$tip/jootraha$total_bill

#Ülesanne 5.1.2 lahendus
ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(breaks = seq(0,1, by=1/15))

#Ülesanne 5.1.3 lahendus
erind <- "Dinner ehk õhtusöök"

###%Ülesanne 6.1.1 lahendus
ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))
#VASTUS: Õige vastus peaks olema 3 (tulbad on punast tooni)

###%Ülesanne 7.1.1 lahendus
j1 <- ggplot(jootraha, aes(x = size, y = ratio, group=size)) +
  geom_boxplot()
j1

#Ülesanne 7.1.2 lahendus
j1 + labs(x="laudkonna suurus", y="suhe jootraha ja arve vahel")

#Ülesanne 7.1.3 lahendus
tabel <- table(jootraha$size)
tabel

###%Ülesanne 8.1.1
# tunnuste nimed andmestikus (total_bill on arve, tip on jootraha dollarites, size laudkonna suurus)
names(jootraha)
jootraha$tip.per.person <- jootraha$tip/jootraha$size

#Ülesanne 8.1.2 
ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 9.1
ggplot(jootraha, aes(x = size, y = tip.per.person, fill=sex,
                     color=sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)

###%Ülesanne 10.1 
ggplot(jootraha, aes(x = size, y = tip.per.person, fill=sex,
                     linetype=sex)) + 
  stat_summary(geom = "line",  fun.y = mean)  + 
  scale_x_continuous(breaks = 1:6)
