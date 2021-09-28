NSW <- read.csv("C:\\Users\\deeks\\Documents\\MIDS\\IDS 702_Modeling and representation of data\\Team Assignments\\Datasets\\lalondedata.txt")

NSW$treat <- factor(NSW$treat)
NSW$hispan <- factor(NSW$hispan)
NSW$black <- factor(NSW$black)
NSW$married <- factor(NSW$married)
NSW$nodegree <- factor(NSW$nodegree)
NSW$wage_diff <- NSW$re78 - NSW$re74
NSW$agec <- NSW$age - mean(NSW$age)
NSW$educ_c <- NSW$educ - mean(NSW$educ)

ggplot(NSW,aes(x=wage_diff)) + geom_histogram()

modelbase <- lm(formula = wage_diff ~ treat + agec + black + hispan + educ + married + nodegree, data = NSW)
summary(modelbase)

# Linearity assumptions for baseline model
# now we use plot residuals to check for transformations
ggplot(NSW, aes(x=age ,y=modelbase$residual))+ geom_point() + geom_smooth() + ggtitle("Age vs Residuals")
# we see that model does well on linearity with age 

# educ vs residual -- roughly random and linearity satisfied but might consider transformation if desperate
ggplot(NSW, aes(x=educ ,y=modelbase$residual))+ geom_point() + geom_smooth() + ggtitle("Educ vs Residuals")

# now checking for independence, constant variance and outliers, we see point 132 as outlier
# but it is not an influential point
plot(modelbase, which =1)
plot(modelbase , which =2)
plot(modelbase, which =4)
plot(modelbase, which =5)

# now we do stepwise selection
null_model <- lm(formula = wage_diff ~ treat + agec + black + hispan + treat:agec + treat:hispan + treat:black + agec:black + agec:hispan, data = NSW)
full_model <- lm(formula = wage_diff ~ treat + agec + black + hispan + educ_c + married + nodegree + treat:agec + treat:hispan + treat:black + agec:black + agec:hispan + educ_c:treat + educ_c:hispan, data = NSW)
Model_stepwise <- step(null_model, scope = formula(full_model), direction="both",trace=0)
Model_stepwise$call

AIC <- lm(formula = wage_diff ~ treat + agec + married + treat:agec, data = NSW)
BIC <- lm(formula = wage_diff ~ treat + agec + treat:agec, data = NSW)
anova(BIC, AIC)
# AIC and BIC give same variables except married; by taking into account the EDA 
# and comparing the two with annova, we choose AIC model as the main model
summary(AIC)
summary(full_model)
# Just to confirm we add each interaction on the null model and two we found interesting in the EDA one by one
# and do an anova test and see that none of them significantly improved our fit for the model
# so we keep AIC (main model) as our final model

main_model <- lm(formula = wage_diff ~ treat + age + married + treat:age, data = NSW)
test_model <- lm(formula = wage_diff ~ treat + age + married + treat:age + nodegree, data = NSW)
anova(main_model, test_model)

# Given that our main model is final model we move on to model assessment and interpreations and we are screwed

main_model <- lm(formula = wage_diff ~ agec + treat + married + agec:treat, data = NSW)
summary(main_model)
library(ggplot2)
ggplot(NSW, aes(x=agec, y = wage_diff)) + geom_point() + geom_smooth()
# roughly linear for age


plot(main_model, which = 1)
# constant variance and independence but we need to check outliers
plot(main_model, which = 2)
# noramlity seems fine


plot(main_model, which =5)
# we have few few leverage points given their leverage is greater than 0.02

# so now we have leverage and outliers and we check the cook's distance to ensure that none of 
# them are influential points

plot(main_model, 4)
# we have no influential points 

# lastly we check for multivollinearity
library(rms)
vif(main_model)

# Removing obs 132
`%!in%` <- Negate(`%in%`)
NSW_no_outlier <- NSW[rownames(NSW) %!in% c(132), ]
ggplot(NSW_no_outlier,aes(x=wage_diff)) + geom_histogram()

main_model_no_outlier <- lm(formula = wage_diff ~ age + treat + married + treat:age, data = NSW_no_outlier)
summary(main_model_no_outlier)
ggplot(NSW_no_outlier, aes(x=agec, y = wage_diff)) + geom_point() + geom_smooth()
plot(main_model_no_outlier, which = 1)
plot(main_model_no_outlier, which = 2)
plot(main_model_no_outlier, which = 4)
plot(main_model_no_outlier, which = 5)




