###% Ülesanne 1.1.1
library(reshape2)
jootraha <-tips
jootraha$day <- factor(jootraha$day, levels = c("Thur", "Fri", "Sat", "Sun"))

summary(jootraha)

library(ggplot2)

# Ülesanne 1.1.2

j1 <- ggplot(jootraha, aes(x = day)) + 
  geom_bar(fill = "royalblue")  + 
  scale_x_discrete(name = "Day of Week", limits = c("Thur", "Fri", "Sat", "Sun"), 
                   labels = c("thursday", "friday", "saturday", "sunday")) + 
                   ylab(label = "Counts")

j1


###% Ülesanne 2.1.1 

names(jootraha)

library(scales)
j1 <- ggplot(jootraha, aes(x = day)) + 
           geom_bar(aes(fill = time), position = "fill") + 
           scale_y_discrete(name = "Percentage") + 
            scale_fill_hue("Time") + 
            xlab(label = "Day")

j1


###% Ülesanne 3.1.1

names (jootraha)

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + 
             geom_point(aes(colour = day)) +
             scale_color_hue(limits = c("Thur", "Fri", "Sat", "Sun"))
j1

# Ülesanne 3.1.2

# suurim <- "Sat"


###% Ülesanne 4.1.1

j1 <- ggplot(jootraha, aes(x = total_bill, y = tip)) + geom_point(aes(color = sex, shape = smoker))
j1

# Ülesanne 4.1.2

j1 + geom_smooth(method = lm)


###% Ülesanne 5.1.1

jootraha$ratio <- factor(jootraha$tip / jootraha$total_bill)

# Ülesanne 5.1.2

ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(bins = 15)

# Ülesanne 5.1.3

erind <- "Dinner"
  

###% Ülesanne 6.1 

ggplot(jootraha, aes(x = ratio, y = ..density..)) + geom_histogram(aes(fill = "chartreuse")) 
# mingil põhjusel kood ei tööta
  

###% Ülesanne 7.1.1 

j1 <- ggplot(jootraha, aes(x = tip, y = total_bill, group = size)) + geom_boxplot()
j1  

# Ülesanne 7.1.2.

j1 + labs(x = "laudkonna suurus", y = "jootraha ja arve suhe")

# Ülesanne 7.1.3

tabel <- table(size)
tabel  

###% Ülesanne 8.1.1

names(jootraha)

jootraha$tip.per.person <- factor(jootraha$tip / jootraha$size)

# Ülesanne 8.1.2

ggplot(jootraha, aes(x = tip.per.person, y = size)) + 
  stat_summary(geom = "line", fun = "mean") + 
                 scale_x_discrete(breaks = 1:6)

###% Ülesanne 9.1.1

ggplot(jootraha, aes(x = tip.per.person, y = size, color = sex)) + 
  stat_summary(geom = "line", fun = "mean") + 
  scale_x_discrete(breaks = 1:6)

###% Ülesanne 10.1.1

ggplot(jootraha, aes(x = tip.per.person, y = size, linetype = sex)) + 
  stat_summary(geom = "line", fun = "mean") + 
  scale_x_discrete(breaks = 1:6)








