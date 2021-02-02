###% ylesanne 1.1 lahendus, IV kodutöö

library(reshape2)
jootraha <-tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))
summary(jootraha)

#1
library(ggplot2)

#2
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill = "royalblue") + 
  scale_x_discrete(name = "Day of week", 
                   limits = c("Thur", "Fri", "Sat", "Sun"), 
                   labels = c("thursday", "friday", "saturday", "sunday")) +
  ylab(label = "Counts")
j1

###% ylesanne 2.1 lahendus

library(scales)
j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = percent) + 
  scale_fill_hue(name = "Time") + 
  xlab(label = "Day")
j1

###% ylesanne 3.1 lahendus

names(jootraha)

#1
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = day)) +
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))

j1

#2
suurim  <- "Sat"

###% ylesanne 4.1 lahendus

#1
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(color = sex, shape = smoker)) 
j1

#2
j1 + geom_smooth(method = lm)

###% ylesanne 5.1 lahendus

#1
jootraha$ratio <- jootraha$tip/jootraha$total_bill

#2
ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins = 15)

#3
erind <- "Dinner"

###% ylesanne 6.1 lahendus

# 3

###% ylesanne 7.1 lahendus

#1
j1 <- ggplot(jootraha, aes(x = size, y = ratio, group = size)) + geom_boxplot( )
j1

#2
j1 + labs(x = "laudkonna suurus",  y = "jootraha ja arve suhe")

#3
tabel <- table(jootraha$size)
tabel

###% ylesanne 8.1 lahendus

#1
jootraha$tip.per.person <- jootraha$tip/jootraha$size

#2
ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun.y = mean) + 
  scale_x_continuous(breaks = 1:6)

###% ylesanne 9.1 lahendus

ggplot(jootraha, aes(x = size, y = tip.per.person, color = sex)) +
  stat_summary(geom = "line",  fun.y = mean) + 
  scale_x_continuous(breaks = 1:6)

###% ylesanne 10.1 lahendus

ggplot(jootraha, aes(x = size, y = tip.per.person, linetype = sex)) + 
  stat_summary(geom = "line",  fun.y = mean) +
  scale_x_continuous(breaks = 1:6)
