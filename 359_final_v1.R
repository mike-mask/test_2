df1 <- read.csv("/Users/devprem/Downloads/Final_Project_359_2021.csv")
View(df1)
str(df1)
library(tidyverse)
df2 <- df1 %>% pivot_longer(-X,
                    names_to = c(".value","Condition"),
                    names_sep = "_")
View(df2)
df3 <- df2 %>%
  select(-X) %>%
  rowid_to_column(var = "Participant ID")
View(df3)
df3$gender_code <- 0
df3$gender_code[df3$gender == "Male"] <- -1
df3$gender_code[df3$gender == "Female"] <- 1
df3$age_centered <- df3$age - mean(df3$age)
df3$CO_centered <- df3$CO - mean(df3$CO)




#review this....


#NOTE to self: there are 3 conditions: red/blue/green. We need to compared each.
#Therefore, compare red+blue and red+green and blue+green
#code note: d=mean diff and the last letter is reference group
#so d_rg means mean diff between red and green and green is ref

#AGE IS A CONTINUOUS VARIABLE!!!!!!!!
#STANDARDIZE IT!@

#codes for green as the reference group
df3$d_rg <- 0
df3$d_bg <- 0
df3$d_rg[df3$Condition == "red"] <- 1
df3$d_bg[df3$Condition == "blue"] <- 1


#codes for red as the reference group
df3$d_gr <- 0
df3$d_br <- 0
df3$d_gr[df3$Condition == "green"] <- 1
df3$d_br[df3$Condition == "blue"] <- 1

#codes for blue as the reference group
df3$d_rb <- 0
df3$d_gb <- 0
df3$d_rb[df3$Condition == "red"] <- 1
df3$d_gb[df3$Condition == "green"] <- 1

#first model uses green as ref
y_green <- lm(y ~ 1 + d_rg + d_bg, data = df3)
CO_green <- lm(CO ~ 1 + d_rg + d_bg, data = df3)
age_green <- lm(age ~ 1 + d_rg + d_bg, data = df3)

sigma.y.green <- summary(y_green)$sigma
sigma.CO.green <- summary(CO_green)$sigma
sigma.age.green <- summary(age_green)$sigma

df3$Z_y_green <- (df3$y - mean(df3$y))/sigma.y.green
df3$Z_CO_green <- (df3$CO - mean(df3$CO))/sigma.CO.green
df3$Z_age_green <- (df3$age - mean(df3$age))/sigma.age.green

#Model_Z_green will give d for red+green and for blue+green
Model_Z_green <- lm(Z_y_green ~ 1 + Z_CO_green + Z_age_green + d_rg + d_bg + gender_code, data=df3)
summary(Model_Z_green)

#SECOND MODEL USES RED AS REFERENCE GROUP
y_red <- lm(y ~ 1 + d_gr + d_br, data = df3)
CO_red <- lm(CO ~ 1 + d_gr + d_br, data=df3)
age_red <- lm(age ~ 1 + d_gr + d_br, data=df3)

sigma.y.red <- summary(y_red)$sigma
sigma.CO.red <- summary(CO_red)$sigma
sigma.age.red <- summary(age_red)$sigma

df3$Z_y_red <- (df3$y - mean(df3$y))/sigma.y.red
df3$Z_CO_red <- (df3$CO - mean(df3$CO))/sigma.CO.red
df3$Z_age_red <- (df3$age - mean(df3$age))/sigma.age.red

#Model_Z_red will give d for green+red and blue+red
Model_Z_red <- lm(Z_y_red ~ 1 + Z_CO_red + Z_age_red + d_gr + d_br + gender_code, data=df3)
summary(Model_Z_red)
sigma.age.red
sigma.age.green


#NOTE TO SELF: you only need to get the sigmas once and you only need to standardize
#everything once... no need to do it this way...
#but do once just as a test...
summary(Model_Z_green)
summary(Model_Z_red)

df3 %>%
  filter(Condition == "green") %>%
  summarize(mean_green = mean(y))

df3 %>%
  filter(Condition == "red") %>%
  summarize(mean_red = mean(y))

df3 %>%
  filter(Condition == "blue") %>%
  summarize(mean_blue = mean(y))

#My prediction:
#red is 1.5 SD higher than green
#blue is -.9 SD lower than green
#blue is 2.4 SD lower than red
#so: blue is lowest, green is middle, red is highest
#CORRECT =)
library(boot)
library(parallel)
#now simply run the bootstrap on these 2 models and you're done =)
Original.Mean.CO <- mean(df3$CO)
Original.Mean.Age <- mean(df3$age)
set.seed(123)
Boot.EffectSizes <- function(data,indices) {
  bootdata <- data[indices,]

  y_green <- lm(y ~ 1 + d_rg + d_bg, data = bootdata)
  CO_green <- lm(CO ~ 1 + d_rg + d_bg, data = bootdata)
  age_green <- lm(age ~ 1 + d_rg + d_bg, data = bootdata)

  sigma.y.green <- summary(y_green)$sigma
  sigma.CO.green <- summary(CO_green)$sigma
  sigma.age.green <- summary(age_green)$sigma

  bootdata$Z_y_green <- (bootdata$y - mean(bootdata$y))/sigma.y.green
  bootdata$Z_CO_green <- (bootdata$CO - Original.Mean.CO)/sigma.CO.green
  bootdata$Z_age_green <- (bootdata$age - Original.Mean.Age)/sigma.age.green

  #Model_Z_green will give d for red+green and for blue+green
  Model_Z_green <- lm(Z_y_green ~ 1 + Z_CO_green + Z_age_green + d_rg + d_bg + gender_code, data=bootdata)



  #SECOND MODEL USES RED AS REFERENCE GROUP
  y_red <- lm(y ~ 1 + d_gr + d_br, data = bootdata)
  CO_red <- lm(CO ~ 1 + d_gr + d_br, data=bootdata)
  age_red <- lm(age ~ 1 + d_gr + d_br, data=bootdata)

  sigma.y.red <- summary(y_red)$sigma
  sigma.CO.red <- summary(CO_red)$sigma
  sigma.age.red <- summary(age_red)$sigma

  bootdata$Z_y_red <- (bootdata$y - mean(bootdata$y))/sigma.y.red
  bootdata$Z_CO_red <- (bootdata$CO - Original.Mean.CO)/sigma.CO.red
  bootdata$Z_age_red <- (bootdata$age - Original.Mean.Age)/sigma.age.red

  #Model_Z_red will give d for green+red and blue+red
  Model_Z_red <- lm(Z_y_red ~ 1 + Z_CO_red + Z_age_red + d_gr + d_br + gender_code, data=bootdata)

  Model_Green_Effects <- coef(Model_Z_green)
  Model_Red_Effects <- coef(Model_Z_red)
  d_rg <- Model_Green_Effects[4]
  d_bg <- Model_Green_Effects[5]
  beta <- Model_Green_Effects[2]
  d_br <- Model_Red_Effects[5]
  #What I need
  c(d_rg, d_bg, d_br, beta)

}

stdboot <- boot(data=df3, statistic= Boot.EffectSizes, R=99999, parallel="multicore", ncpus=detectCores()-1)

stdboot
summary(Model_Z_green)
summary(Model_Z_red)

boot.ci(stdboot, index=1, conf=(0.95), type="bca")
boot.ci(stdboot, index=2, conf=(0.95), type="bca")
boot.ci(stdboot, index=3, conf=(0.95), type="bca")
boot.ci(stdboot, index=4, conf=(0.95), type="bca")

summary(lm(CO ~ 1 + d_rg + d_bg, data=df3))


#Graph Time

Full.Unstandardized.Model <- lm(y ~ 1 + CO_centered + age_centered + d_rg + d_bg + gender_code, data=df3)
summary(Full.Unstandardized.Model)
b <- coef(Full.Unstandardized.Model)
b

#GRAPH!!!!!

group.green.b0 <-b[1]
group.green.b1 <-b[2]
group.red.b0 <- b[4] + b[1]
group.red.b1 <- b[2]
group.blue.b0 <- b[5] + b[1]
group.blue.b1 <- b[2]


group.green.color <- "#117733"
group.red.color <- "#882255"
group.blue.color <- "#332288"

library(psych)
describe(df3)[,c(1:10)]
#covariate range is c(-2.94, 3.16), DV is c(3.10, 10)

plot(df3$CO_centered[df3$Condition == "green"],df3$y[df3$Condition == "green"],
     type="p", xlab="Conscientiousness (Centered)", ylab="Employee Performance",
     main=NA, xlim=c(-2.94, 3.16), ylim=c(3.1,10),
     col=group.green.color, frame.plot=FALSE, pch=15, cex=.8)
par(new=TRUE)
plot(df3$CO_centered[df3$Condition == "red"], df3$y[df3$Condition == "red"],
     type="p", xlab=NA, ylab=NA,
     main=NA, xlim=c(-2.94, 3.16), ylim=c(3.1,10),
     col=group.red.color, frame.plot=FALSE, axes=FALSE, pch=20, cex=.8)
par(new=TRUE)
plot(df3$CO_centered[df3$Condition == "blue"], df3$y[df3$Condition == "blue"],
     type="p", xlab=NA, ylab=NA,
     main=NA, xlim=c(-2.94, 3.16), ylim=c(3.1,10),
     col=group.blue.color, frame.plot=FALSE, axes=FALSE, pch=1, cex=.8)

minGreen <- min(df3$CO_centered[df3$Condition == "green"])
maxGreen <- max(df3$CO_centered[df3$Condition == "green"])
minRed <- min(df3$CO_centered[df3$Condition == "red"])
maxRed <- max(df3$CO_centered[df3$Condition == "red"])
minBlue <- min(df3$CO_centered[df3$Condition == "blue"])
maxBlue <- max(df3$CO_centered[df3$Condition == "blue"])

segments(minGreen, group.green.b0 + minGreen*group.green.b1,
         maxGreen, group.green.b0 + maxGreen*group.green.b1,
         lty=1, col=group.green.color)
segments(minRed, group.red.b0 + minRed*group.red.b1,
         maxRed, group.red.b0 + maxRed*group.red.b1,
         lty=1, col=group.red.color)
segments(minBlue, group.blue.b0 + minBlue*group.blue.b1,
         maxBlue, group.blue.b0 + maxBlue*group.blue.b1,
         lty=1, col=group.blue.color)

segments(0,0,0,11,lty=1,lwd=1.5)

text(4, 6.5, "Green Condition", adj=c(0,0), cex=.8, col=group.green.color)
text(3.5,5.2, "Red Condition", adj=c(0,0), cex=.8, col=group.red.color)
text(5.5,6.0, "Blue Condition", adj=c(0,0), cex=.8, col=group.blue.color)


#Atttemping to graph with ggplot.......
set.seed(777)
ggplot(df3, aes(x=CO_centered, y=y, shape=Condition, color=Condition, show.legend=FALSE))+
  geom_jitter(height=0.1, width=0.1, size=1.25, show.legend=FALSE)+
  geom_smooth(method="lm", size=0.75)+
  labs(x="Conscientiousness (Centered)", y="Employee Performance")+
  scale_color_manual(labels=c("Blue","Green","Red"), values=c("#332288","#117733","#AA4499"))+
    scale_shape_manual(values=c(15,16,17)+
    theme_classic()+
  geom_segment(aes(x=0,y=4, xend=0,yend=10.5),size=0.1, color="black")+
  scale_x_continuous(limits=c(-3.25,3.25), breaks=c(-3,-2,-1,0,1,2,3))+
  scale_y_continuous(limits=c(3,10.5), breaks=c(4,6,8,10))+
  guides(color=guide_legend(reverse=TRUE))


  set.seed(777)
  ggplot(df3, aes(x=CO_centered, y=y, shape=Condition, color=Condition, show.legend=FALSE))+
    geom_jitter(height=0.1, width=0.1, size=1.25, show.legend=FALSE)+
    geom_smooth(method="lm", size=0.75)+
    labs(x="Conscientiousness (Centered)", y="Employee Performance")+
    scale_color_manual(labels=c("Blue","Green","Red"), values=c("#332288","#117733","#AA4499"))+
    scale_shape_manual(values=c(15,16,17))+
    theme_classic()+
    geom_segment(aes(x=0,y=4, xend=0,yend=10.5),size=0.1, color="black")+
    scale_x_continuous(limits=c(-3.25,3.25), breaks=c(-3,-2,-1,0,1,2,3))+
    scale_y_continuous(limits=c(3,10.5), breaks=c(4,6,8,10))+
    guides(color=guide_legend(reverse=TRUE))




  describe(df3[,c(1:10)])
ggplot(df3, aes(x=CO_centered, y=y))


  #aes(data=subset(df3,Condition=="green"), color="#117733", size=1, shape=16)),
  (data=subset(df3,Condition=="red"), color="#882255", size=1, shape=15),
  (data=subset(df3, Condition=="blue"), color="#332288", size=1, shape=17)+

?ggplot

  df3 %>%
    filter(Condition == "blue") %>%
    count()


  #Winsorizing TEST


  #formula=df3$y ~ 1 + df3$CO_centered + df3$age_centered + df3$d_rg + df3$d_bg + df3$gender_code)+



