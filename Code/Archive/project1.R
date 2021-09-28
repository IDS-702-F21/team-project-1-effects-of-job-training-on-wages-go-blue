rm(list = ls())
getwd()
# setwd("Desktop/Duke/MIDS/courses/702_modeling and representation of data/dataset/")

# install.packages()
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)

lal <- read.csv("lalondedata.txt", header=T)
head(lal)
dim(lal)
summary(lal)
str(lal)
lal <- read.csv("lalondedata.txt",header=T,
                    colClasses=c("factor","factor","numeric","numeric",
                                 "factor", "factor", "factor", "factor", 
                                 "numeric", "numeric", "numeric"))
lal$re78Bi <- 0
lal$re78Bi[lal$re78 > 0] <- 1
lal$re78Bi_F <- "Zero W"
lal$re78Bi_F[lal$re78Bi == 1] <- "NonZero W"
lal$re78Bi_F <- factor(lal$re78Bi_F)

lal$educ_F <- "1.Elementary"
lal$educ_F[lal$educ >= 6 & lal$educ < 10] <- "2.Middle"
lal$educ_F[lal$educ >= 10 & lal$educ < 13] <- "3.High School"
lal$educ_F[lal$educ >= 13] <- "4.After high"
lal$educ_F <- factor(lal$educ_F)

str(lal)
table(lal$educ)

####### Q2 #######
### EDA
## boxplots for the numeric variables
# age vs re78Bi_F
ggplot(lal,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Wage",
       x="Wage?",y="Age") + 
  theme_classic() + theme(legend.position="none") 

# educ vs re78Bi_F
ggplot(lal,aes(x=re78Bi_F, y=educ, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Educ vs Wage",
       x="Wage?",y="Educ") + 
  theme_classic() + theme(legend.position="none") #+
# Educ indeed has an effect

# re74 vs re78Bi_F
ggplot(lal,aes(x=re78Bi_F, y=re74, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Previous Wage vs Wage",
       x="Wage?",y="Previous Wage") + 
  theme_classic() + theme(legend.position="none") #+
# Previous wage indeed has an effect

## tables for the factor variables
# treat vs re78Bi_F
table(lal[,c("re78Bi_F","treat")])
# table(lal[,c("re78Bi_F","treat")])/sum(table(lal[,c("re78Bi_F","treat")]))
apply(table(lal[,c("re78Bi_F","treat")])/sum(table(lal[,c("re78Bi_F","treat")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$treat, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","treat")]))
# surprisingly have no effect!

# black vs re78Bi_F
table(lal[,c("re78Bi_F","black")])
apply(table(lal[,c("re78Bi_F","black")])/sum(table(lal[,c("re78Bi_F","black")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$black, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","black")]))
# black indeed has an effect

# hispan vs re78Bi_F
table(lal[,c("re78Bi_F","hispan")])
apply(table(lal[,c("re78Bi_F","hispan")])/sum(table(lal[,c("re78Bi_F","hispan")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$hispan, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","hispan")]))

# married vs re78Bi_F
table(lal[,c("re78Bi_F","married")])
apply(table(lal[,c("re78Bi_F","married")])/sum(table(lal[,c("re78Bi_F","married")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$married, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","married")]))

# nodegree vs re78Bi_F
table(lal[,c("re78Bi_F","nodegree")])
apply(table(lal[,c("re78Bi_F","nodegree")])/sum(table(lal[,c("re78Bi_F","nodegree")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$nodegree, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","nodegree")]))

# educ_F vs re78Bi_F
table(lal[,c("re78Bi_F","educ_F")])
apply(table(lal[,c("re78Bi_F","educ_F")])/sum(table(lal[,c("re78Bi_F","educ_F")])),
      2,function(x) x/sum(x)) 
tapply(lal$re78Bi_F, lal$educ_F, function(x) table(x)/sum(table(x)))
chisq.test(table(lal[,c("re78Bi_F","educ_F")]))

## binnedplots of continuous predictors versus re78Bi
par(mfrow=c(1,1)) 
# age vs re78Bi_F
binnedplot(y=lal$re78Bi,lal$age,xlab="Age",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Age and Non-zero Wage cases",
           col.int="white") 
# seems no obvious trend, slightly went down

# educ vs re78Bi_F
binnedplot(y=lal$re78Bi,lal$educ,xlab="Educ",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Educ and Non-zero Wage cases",
           col.int="white") 
# roughly linearly incease trend as expected

# re74 vs re78Bi_F
binnedplot(y=lal$re78Bi,lal$re74,xlab="Previous Wage",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Previous Wage and Non-zero Wage cases",
           col.int="white") 
# roughly linearly incease trend as expected

library(GGally)
ggpairs(lal[,!is.element(colnames(lal),
                         c("treat","X","re78Bi","educ_F"))],
        mapping=ggplot2::aes(colour = "red4",alpha=0.6))

## short brief of observations:
# shall include in the base model: educ, re74, treat (as question wants), black

## interaction
# age vs re78Bi_F by educ
table(lal$age)
summary(lal)
ggplot(lal,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Non-zero Wages, by Educ",
       x="Non-zero Wages?",y="Age") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ educ)
# shall investigate the interaction 

# re74 vs re78Bi_F by treat
ggplot(lal,aes(x=re78Bi_F, y=re74, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Previous Wage vs Non-zero Wages, by treat",
       x="Non-zero Wages?",y="Previous Wage") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ treat)
# this can be included in the full model (try stay away from log to avoid loss
# observation)

str(lal)
# educ vs re78Bi by black
par(mfcol=c(2,1))
table(lal$educ)
#first plot for black = 0
binnedplot(lal$educ[lal$black=="0"], y=lal$re78Bi[lal$black=="0"], 
           xlab = "Educ", ylab = "Non-zero Wage", 
           main = "Binned Educ and Non-zero Wage cases (Black = 0)") 

#next the plot for black = 1
binnedplot(lal$educ[lal$black=="1"], y=lal$re78Bi[lal$black=="1"], 
           xlab = "Educ", ylab = "Non-zero Wage", 
           main = "Binned Educ and Non-zero Wage cases (Black = 1)") 
# shall investigate the interaction as linear increase trend disappear 
# when black = 1

# educ_F vs re78Bi_F by black
lal_black0 <- lal[lal$black=="0",]
lal_black1 <- lal[lal$black=="1",]
table(lal$black,lal$educ_F)
apply(table(lal_black0[,c("re78Bi_F", "educ_F")])/sum(table(lal_black0[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x)) 

apply(table(lal_black1[,c("re78Bi_F", "educ_F")])/sum(table(lal_black1[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x))
# shall investigate the interaction

# educ_F vs re78Bi_F by hispan
lal_hispan0 <- lal[lal$hispan=="0",]
lal_hispan1 <- lal[lal$hispan=="1",]
table(lal$hispan,lal$educ_F)
apply(table(lal_hispan0[,c("re78Bi_F", "educ_F")])/sum(table(lal_hispan0[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x)) 

apply(table(lal_hispan1[,c("re78Bi_F", "educ_F")])/sum(table(lal_hispan1[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x))
# given that low observation in elementary and after high for hispan, we shall 
# NOT investigate the interaction

# black vs re78Bi_F by treat
lal_treat0 <- lal[lal$treat=="0",]
lal_treat1 <- lal[lal$treat=="1",]
table(lal$black,lal$treat)
apply(table(lal_treat0[,c("re78Bi_F", "black")])/sum(table(lal_treat0[,c("re78Bi_F", "black")])),
      2,function(x) x/sum(x)) 

apply(table(lal_treat1[,c("re78Bi_F", "black")])/sum(table(lal_treat1[,c("re78Bi_F", "black")])),
      2,function(x) x/sum(x))
# shall investigate the interaction 

# treat vs re78Bi_F by black
lal_black0 <- lal[lal$black=="0",]
lal_black1 <- lal[lal$black=="1",]
table(lal$black,lal$treat)
apply(table(lal_black0[,c("re78Bi_F", "treat")])/sum(table(lal_black0[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 

apply(table(lal_black1[,c("re78Bi_F", "treat")])/sum(table(lal_black1[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 
# (the same as above one) shall investigate the interaction

# treat vs re78Bi_F by hispan
lal_hispan0 <- lal[lal$hispan=="0",]
lal_hispan1 <- lal[lal$hispan=="1",]
table(lal$hispan,lal$treat)
apply(table(lal_hispan0[,c("re78Bi_F", "treat")])/sum(table(lal_hispan0[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 

apply(table(lal_hispan1[,c("re78Bi_F", "treat")])/sum(table(lal_hispan1[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 
# shall investigate the interaction

# age vs re78Bi_F by treat
ggplot(lal,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Wage",
       x="Wage?",y="Age") + 
  theme_classic() + theme(legend.position="none") + facet_wrap(~treat)
# shall investigate the interaction

# brief: interactions shall be considered: 
# treat: hispan, treat:black, educ:black, re74:treat, age:educ, age:treat


