
###% Ülesanne 1.1 
library(reshape2)
jootraha <-tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))

summary(jootraha)
# 1.
install.packages("ggplot2")
library(ggplot2)

# 2.
j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill="royalblue") + 
  scale_x_discrete(name="Day of week",limits = c("Thur", "Fri", "Sat", "Sun"), labels=c("thursday", "friday", "saturday", "sunday"))   +
  ylab(label = "Counts")
j1


###% Ülesanne 2.1 
names(jootraha)

library(scales)
j1 <- ggplot(jootraha, aes(x = day, fill = time)) + 
  geom_bar(position = "fill")  +
  scale_y_continuous(labels = percent) + 
  scale_fill_hue(name  = "Time") +
  xlab(label="Day")
j1
#table(jootraha[,5:6])


###% Ülesanne 3.1
names(jootraha)
# 1.
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = day))  +
  scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
j1


# 2.
jootraha[which.max(jootraha$total_bill), 5]
suurim  <- "Laupäev"


###% Ülesanne 4.1
names(jootraha)
# 1.
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
  geom_point(aes(colour = sex, shape = smoker))
j1

# 2.
j1 + geom_smooth(method = lm, formula = y~x)


###% Ülesanne 5.1
# 1.
jootraha$ratio <- jootraha$tip/jootraha$total_bill
summary(jootraha)

# 2.
ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins=15)

# 3.
jootraha[which.max(jootraha$ratio), ]
erind <- "Dinner"



###% Ülesanne 6.1
ggplot(jootraha, aes(x = ratio, y = ..density..)) + 
  geom_histogram(aes(fill = "chartreuse"))
# Õige vastus: 3


###% Ülesanne 7.1
names(jootraha)
# 1.
j1 <- ggplot(jootraha, aes(x = total_bill, y = tip, group =size)) + geom_boxplot()
j1

# 2.
j1 + labs(x= "laudkonna suurus", y="jootraha ja arve suhe")

# 3.
tabel <- table(jootraha[,"size"])
tabel


###% Ülesanne 8.1
# 1.
jootraha$tip.per.person <- jootraha$tip / jootraha$size
summary(jootraha)

# 2.
ggplot(jootraha, aes(x = size, y = tip.per.person)) + 
  stat_summary(geom = "line",  fun = mean)  + 
  scale_x_continuous(breaks = 1:6)
# Kasutasin fun.y asemel lihtsalt fun-käsklust, sest R ütles, et fun.y on aegunud.


###% Ülesanne 9.1
ggplot(jootraha, aes(x =  size, y = tip.per.person, color = sex)) + 
  stat_summary(geom = "line",  fun = mean)  + 
  scale_x_continuous(breaks = 1:6)


###% Ülesanne 10.1
ggplot(jootraha, aes(x =  size, y = tip.per.person, linetype = sex)) + 
  stat_summary(geom = "line",  fun = mean)  + 
  scale_x_continuous(breaks = 1:6)
