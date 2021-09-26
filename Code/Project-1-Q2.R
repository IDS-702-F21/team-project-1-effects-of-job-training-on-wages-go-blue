rm(list = ls())
getwd()

setwd("/Users/Aarushi/Duke/MIDS - Fall 2021/IDS 702 - Modeling and Data Representation/Team Project -1/")

# install.packages()
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)

nsw <- read.csv("lalondedata.txt", header=T)
head(nsw)
dim(nsw)
summary(nsw)
str(nsw)
nsw <- read.csv("lalondedata.txt",header=T,
                colClasses=c("factor","factor","numeric","numeric",
                             "factor", "factor", "factor", "factor", 
                             "numeric", "numeric", "numeric"))
nsw$re78Bi <- 0
nsw$re78Bi[nsw$re78 > 0] <- 1
nsw$re78Bi_F <- "Zero W"
nsw$re78Bi_F[nsw$re78Bi == 1] <- "NonZero W"
nsw$re78Bi_F <- factor(nsw$re78Bi_F)

nsw$educ_F <- "1.Elementary"
nsw$educ_F[nsw$educ >= 6 & nsw$educ < 10] <- "2.Middle"
nsw$educ_F[nsw$educ >= 10 & nsw$educ < 13] <- "3.High School"
nsw$educ_F[nsw$educ >= 13] <- "4.After high"
nsw$educ_F <- factor(nsw$educ_F)

str(nsw)
table(nsw$educ)

####### Q2 #######
### EDA
## boxplots for the numeric variables
# age vs re78Bi_F
ggplot(nsw,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Wage",
       x="Wage?",y="Age") + 
  theme_classic() + theme(legend.position="none") 

# educ vs re78Bi_F
ggplot(nsw,aes(x=re78Bi_F, y=educ, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Educ vs Wage",
       x="Wage?",y="Educ") + 
  theme_classic() + theme(legend.position="none") #+
# Educ indeed has an effect

# re74 vs re78Bi_F
ggplot(nsw,aes(x=re78Bi_F, y=re74, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Previous Wage vs Wage",
       x="Wage?",y="Previous Wage") + 
  theme_classic() + theme(legend.position="none") #+
# Previous wage indeed has an effect

## tables for the factor variables
# treat vs re78Bi_F
table(nsw[,c("re78Bi_F","treat")])
# table(nsw[,c("re78Bi_F","treat")])/sum(table(nsw[,c("re78Bi_F","treat")]))
apply(table(nsw[,c("re78Bi_F","treat")])/sum(table(nsw[,c("re78Bi_F","treat")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$treat, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","treat")]))
# surprisingly have no effect!

# black vs re78Bi_F
table(nsw[,c("re78Bi_F","black")])
apply(table(nsw[,c("re78Bi_F","black")])/sum(table(nsw[,c("re78Bi_F","black")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$black, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","black")]))
# black indeed has an effect

# hispan vs re78Bi_F
table(nsw[,c("re78Bi_F","hispan")])
apply(table(nsw[,c("re78Bi_F","hispan")])/sum(table(nsw[,c("re78Bi_F","hispan")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$hispan, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","hispan")]))

# married vs re78Bi_F
table(nsw[,c("re78Bi_F","married")])
apply(table(nsw[,c("re78Bi_F","married")])/sum(table(nsw[,c("re78Bi_F","married")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$married, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","married")]))

# nodegree vs re78Bi_F
table(nsw[,c("re78Bi_F","nodegree")])
apply(table(nsw[,c("re78Bi_F","nodegree")])/sum(table(nsw[,c("re78Bi_F","nodegree")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$nodegree, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","nodegree")]))

# educ_F vs re78Bi_F
table(nsw[,c("re78Bi_F","educ_F")])
apply(table(nsw[,c("re78Bi_F","educ_F")])/sum(table(nsw[,c("re78Bi_F","educ_F")])),
      2,function(x) x/sum(x)) 
tapply(nsw$re78Bi_F, nsw$educ_F, function(x) table(x)/sum(table(x)))
chisq.test(table(nsw[,c("re78Bi_F","educ_F")]))

## binnedplots of continuous predictors versus re78Bi
par(mfrow=c(1,1)) 
# age vs re78Bi_F
binnedplot(y=nsw$re78Bi,nsw$age,xlab="Age",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Age and Non-zero Wage cases",
           col.int="white") 
# seems no obvious trend, slightly went down

# educ vs re78Bi_F
binnedplot(y=nsw$re78Bi,nsw$educ,xlab="Educ",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Educ and Non-zero Wage cases",
           col.int="white") 
# roughly linearly incease trend as expected

# re74 vs re78Bi_F
binnedplot(y=nsw$re78Bi,nsw$re74,xlab="Previous Wage",ylim=c(0,1),col.pts="navy",
           ylab ="Non-zero Wage?",main="Binned Previous Wage and Non-zero Wage cases",
           col.int="white") 
# roughly linearly incease trend as expected

library(GGally)
ggpairs(nsw[,!is.element(colnames(nsw),
                         c("treat","X","re78Bi","educ_F"))],
        mapping=ggplot2::aes(colour = "red4",alpha=0.6))

## short brief of observations:
# shall include in the base model: educ, re74, treat (as question wants), black

## interaction
# age vs re78Bi_F by educ
table(nsw$age)
summary(nsw)
ggplot(nsw,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Non-zero Wages, by Educ",
       x="Non-zero Wages?",y="Age") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ educ)
# shall investigate the interaction 

# re74 vs re78Bi_F by treat
ggplot(nsw,aes(x=re78Bi_F, y=re74, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Previous Wage vs Non-zero Wages, by treat",
       x="Non-zero Wages?",y="Previous Wage") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ treat)
# this can be included in the full model (try stay away from log to avoid loss
# observation)

str(nsw)
# educ vs re78Bi by black
par(mfcol=c(2,1))
table(nsw$educ)
#first plot for black = 0
binnedplot(nsw$educ[nsw$black=="0"], y=nsw$re78Bi[nsw$black=="0"], 
           xlab = "Educ", ylab = "Non-zero Wage", 
           main = "Binned Educ and Non-zero Wage cases (Black = 0)") 

#next the plot for black = 1
binnedplot(nsw$educ[nsw$black=="1"], y=nsw$re78Bi[nsw$black=="1"], 
           xlab = "Educ", ylab = "Non-zero Wage", 
           main = "Binned Educ and Non-zero Wage cases (Black = 1)") 
# shall investigate the interaction as linear increase trend disappear 
# when black = 1

# educ_F vs re78Bi_F by black
nsw_black0 <- nsw[nsw$black=="0",]
nsw_black1 <- nsw[nsw$black=="1",]
table(nsw$black,nsw$educ_F)
apply(table(nsw_black0[,c("re78Bi_F", "educ_F")])/sum(table(nsw_black0[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x)) 

apply(table(nsw_black1[,c("re78Bi_F", "educ_F")])/sum(table(nsw_black1[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x))
# shall investigate the interaction

# educ_F vs re78Bi_F by hispan
nsw_hispan0 <- nsw[nsw$hispan=="0",]
nsw_hispan1 <- nsw[nsw$hispan=="1",]
table(nsw$hispan,nsw$educ_F)
apply(table(nsw_hispan0[,c("re78Bi_F", "educ_F")])/sum(table(nsw_hispan0[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x)) 

apply(table(nsw_hispan1[,c("re78Bi_F", "educ_F")])/sum(table(nsw_hispan1[,c("re78Bi_F", "educ_F")])),
      2,function(x) x/sum(x))
# given that low observation in elementary and after high for hispan, we shall 
# NOT investigate the interaction

# black vs re78Bi_F by treat
nsw_treat0 <- nsw[nsw$treat=="0",]
nsw_treat1 <- nsw[nsw$treat=="1",]
table(nsw$black,nsw$treat)
apply(table(nsw_treat0[,c("re78Bi_F", "black")])/sum(table(nsw_treat0[,c("re78Bi_F", "black")])),
      2,function(x) x/sum(x)) 

apply(table(nsw_treat1[,c("re78Bi_F", "black")])/sum(table(nsw_treat1[,c("re78Bi_F", "black")])),
      2,function(x) x/sum(x))
# shall investigate the interaction 

# treat vs re78Bi_F by black
nsw_black0 <- nsw[nsw$black=="0",]
nsw_black1 <- nsw[nsw$black=="1",]
table(nsw$black,nsw$treat)
apply(table(nsw_black0[,c("re78Bi_F", "treat")])/sum(table(nsw_black0[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 

apply(table(nsw_black1[,c("re78Bi_F", "treat")])/sum(table(nsw_black1[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 
# (the same as above one) shall investigate the interaction

# treat vs re78Bi_F by hispan
nsw_hispan0 <- nsw[nsw$hispan=="0",]
nsw_hispan1 <- nsw[nsw$hispan=="1",]
table(nsw$hispan,nsw$treat)
apply(table(nsw_hispan0[,c("re78Bi_F", "treat")])/sum(table(nsw_hispan0[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 

apply(table(nsw_hispan1[,c("re78Bi_F", "treat")])/sum(table(nsw_hispan1[,c("re78Bi_F", "treat")])),
      2,function(x) x/sum(x)) 
# shall investigate the interaction

# age vs re78Bi_F by treat
ggplot(nsw,aes(x=re78Bi_F, y=age, fill=re78Bi_F)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs Wage",
       x="Wage?",y="Age") + 
  theme_classic() + theme(legend.position="none") + facet_wrap(~treat)
# shall investigate the interaction

# brief: interactions shall be considered: 
# treat: hispan, treat:black, educ:black, re74:treat, age:educ, age:treat

################################################################
## Model Building
nsw$age_c <- nsw$age - mean(nsw$age) # Centering age variable for better interpretation

#Model 1 - Main effects (Factor Education)
Model_1 <- glm(re78Bi_F ~ educ_F + black + hispan + treat + re74 + age_c + 
                 married, data = nsw, family = binomial)

summary(Model_1)
# Model 1 - considers all the main effects (excluding No degree) and education as factor)
# At 95% significance Intercept,Black, Re74 and Age_c are significant
# AIC = 651.24

#Model 2 - Main effects (Numeric Education)
Model_2 <- glm(re78Bi_F ~ educ + black + hispan + treat + re74 + age_c + 
                 married, data = nsw, family = binomial) # FINAL BASE MODEL

summary(Model_2)
# Model 1 - considers all the main effects (excluding No degree)and education as numeric)
# At 95% significance Black, Re74 and Age_c are significant
# AIC = 650.95

#Both models are pretty much the same. Factoring education does not really make a difference. 
# The AIC values only differ by 1. Since its lower for model 2 i think we should proceed with that

#Model Assessment

#save the raw residuals
rawresid1 <- residuals(Model_2,"resp")

#binned residual plots - model
binnedplot(x=fitted(Model_2),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

# For significant coeffs
#binned residual plots - centered Age
binnedplot(x=nsw$age_c,y=rawresid1,xlab="Age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#no trend 2 points outside

#binned residual plots - Re74
binnedplot(x=nsw$re74,y=rawresid1,xlab="Re74",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#not as much of a trend 2 points outside

# Model validation

#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Model_2) >= 0.5, "1","0")),
                            as.factor(nsw$re78Bi),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#Maybe we can try to increase that accuracy.
#Also, the TNR looks low here.

#first, let's repeat with the marginal percentage in the data

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Model_2) >= 0.216, "1","0")),
                            as.factor(nsw$re78Bi),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#huge difference!  seems a lot of predicted probabilities are in the .5 to .58  range, so cutoff matters.
#either way, we have large off-diagonal numbers. specificity is sensitive to the cutoff

#look at ROC curve
roc(nsw$re78Bi_F,fitted(Model_2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#pretty tight to the line -- not a strongly predictive logistic regression

#Looking at interactions

Model_3 = glm(re78Bi_F ~ treat*black  + treat*hispan + re74*treat + educ*black + age_c*treat + age_c*educ,
              data = nsw, family = binomial)
summary(Model_3)

#change in deviance tests to see if the full set of interactions are useful.
anova(Model_2, Model_3, test= "Chisq")

## Interactions are significant. Adding the interactions did help. We should check which one is significant
# We remove educ and black to see if it is significant given the others in the model
Model_3a = glm(re78Bi_F ~ treat*black  + treat*hispan + re74*treat + age*treat + age_c*educ,
              data = nsw, family = binomial)
anova(Model_3, Model_3a, test= "Chisq")

# not significant, so we can drop it.

# Next look at the other interactions 
# re74*treat

Model_3b = glm(re78Bi_F ~ treat*black  + treat*hispan + age*treat + age_c*educ,
              data = nsw, family = binomial)

anova(Model_3b, Model_3a, test= "Chisq")
# It is significant so we can retain it

# Interaction between treat*black
Model_3c = glm(re78Bi_F ~  treat*hispan + age*treat + age_c*educ + re74*treat,
               data = nsw, family = binomial)

anova(Model_3c, Model_3a, test= "Chisq")
# Not significant

# Interaction between treat*hispan
Model_3d = glm(re78Bi_F ~  treat*black + age*treat + age_c*educ + re74*treat,
               data = nsw, family = binomial)

anova(Model_3d, Model_3a, test= "Chisq")
# Not significant

# Interaction between age*educ
Model_3e = glm(re78Bi_F ~  treat*black +treat*hispan + age_c*treat + re74*treat,
               data = nsw, family = binomial)

anova(Model_3e, Model_3a, test= "Chisq")


#Interaction between age* and treat was significant in the model so we retain it. 
#Interaction between treat and race is relevant to our question so for now we retain that too.

#Final Model:
Model_4 = glm(re78Bi_F ~educ + treat*black + treat*hispan + re74*treat + age_c*treat + 
                married,data = nsw, family = binomial)

Model_4a = glm(re78Bi_F ~educ + treat + black + hispan + re74 + age_c + 
                married + treat:black + treat:hispan + re74:treat + age_c:treat
              ,data = nsw, family = binomial)

summary(Model_4a)

#let's use the stepwise function to do model selection (using BIC)
n <- nrow(nsw)
#null_model <- glm(re78Bi_F~ educ + black + hispan + treat + re74 + age_c + 
#                    married,data=nsw,family=binomial) #Only main effects

null_model <- glm(re78Bi_F~ treat,data=nsw,family=binomial)

full_model <- glm(re78Bi_F ~ treat*black  + treat*hispan + re74*treat + educ*black + age_c*treat 
                  + age_c*educ + married, data = nsw, family = binomial)
  
  
 # # glm(re78Bi_F~ educ*black + educ*hispan + educ*treat + educ*re74 + educ*age_c + educ*married
 #                 + black:treat + black*re74 + black*age_c+ black*married
 #                 + hispan:treat + hispan*re74 + hispan*age_c+ hispan*married
 #                 + treat*re74 + treat*age_c + treat*married
 #                 + re74*age_c + re74*married
 #                 + age_c*married, data=nsw,family=binomial) #

AIC_stepwise <- step(null_model, scope = formula(full_model),direction="both",trace=0)
AIC_stepwise


#glm(formula = re78Bi_F ~ treat + black + re74 + age_c + treat:age_c + 
#treat:black + treat:re74, family = binomial, data = nsw)

# glm(formula = re78Bi_F ~ re74 + age_c, family = binomial, data = nsw)
#Does not include any interactions

BIC <- step(null_model,scope= formula(full_model),direction="both",
     trace=0,k = log(n))
BIC
#Call:  glm(formula = re78Bi_F ~ re74 + age_c, family = binomial, data = nsw)

model <- glm(formula = re78Bi_F ~ black + treat + re74 + age_c + treat:age_c, data=nsw, family = binomial)
summary(model)
model_aic<- glm(formula = re78Bi_F ~ black + treat + re74 + age_c, data=nsw, family = binomial)
summary(model_aic)

anova(model,model_aic,test = "Chisq")

#Call:  glm(formula = re78Bi_F ~ treat + re74 + age_c + treat:age_c, family = binomial, data = nsw)

# We can keep the interactions included in model_4 because we will interpret 
# them even though they arent significant



# Binned residual plots with this perhaps final model
Final_Model <- glm(formula = re78Bi_F ~ treat + age_c + re74 + black + treat:age_c + 
                      treat:black + treat:re74, family = binomial, data = nsw)

roc(nsw$re78Bi_F,fitted(Final_Model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#marginal percentage in the data
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Final_Model) >= 0.229, "1","0")),
                            as.factor(nsw$re78Bi),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]

summary(Final_Model)

rawresid4 <- residuals(Final_Model,"resp")

par(mfcol=c(1,1))
# Only for Deekshita
binnedplot(x=fitted(Final_Model),y=rawresid4,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot_1",col.pts="navy")

# Real Annual Earnings in 1974
binnedplot(x=nsw$re74,y=rawresid4,xlab="re74 Earnings",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# Centered Age
binnedplot(x=nsw$age_c,y=rawresid4,xlab="Age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

#Transformations
nsw$age_cb <- (nsw$age)^3
nsw$age_sq <- (nsw$age)^2
#Final_Model_0 <- glm(formula = re78Bi_F ~ treat + age + age_sq + re74 + black + treat:age_sq + 
 #                    treat:black + treat:re74, family = binomial, data = nsw)
# To incorporate based on Michael's reposnse


Final_Model_1 <- glm(formula = re78Bi_F ~ treat + age + age_sq + age_cb + re74 + black + treat:age + 
                        treat:black + treat:re74, family = binomial, data = nsw)
summary(Final_Model_1)
rawresid5 <- residuals(Final_Model_1,"resp")
binnedplot(x=fitted(Final_Model_1),y=rawresid5,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot_2",col.pts="navy")
#Deal with the point on the end later (??) - High Leverage point

binnedplot(x=nsw$age,y=rawresid5,xlab="Age",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=nsw$age_sq,y=rawresid5,xlab="Age squared",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
binnedplot(x=nsw$age_cb,y=rawresid5,xlab="Age Cubed",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

#let's do the confusion matrix
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Final_Model_1) >= 0.5, "1","0")),
                            as.factor(nsw$re78Bi),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#let's repeat with the marginal percentage in the data
Conf_mat_FM1 <- confusionMatrix(as.factor(ifelse(fitted(Final_Model_1) >= 0.250, "1","0")),
                            as.factor(nsw$re78Bi),positive = "1")
Conf_mat_FM1$table
Conf_mat_FM1$overall["Accuracy"];
Conf_mat_FM1$byClass[c("Sensitivity","Specificity")]
#still not moving much.... the model can predict only so well

table(nsw$re78Bi_F)

#ROC curve...
roc(nsw$re78Bi,fitted(Final_Model_1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#Confidence interval
confint.default(Model_4)   #on log odds scale
exp(confint.default(Model_4))
