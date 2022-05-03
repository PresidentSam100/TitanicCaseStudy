#A Study on the Survival Rates of Titanic Passengers
#By Sam Lu and Luke Venkataramanan
#Data Source: https://www.kaggle.com/vinicius150987/titanic3

#------------------------------------------------------------------------------#

#Required packages for certain functions and graphs
require(rms)
require(rstan)
require(plyr)

data <- read.csv(file.choose(), header=T) #Load titanic 3 data

#------------------------------------------------------------------------------#

#List of variables to analyze
v <- c('pclass', 'survived', 'age', 'sex', 'sibsp', 'parch')
v

t3 <- data[, v]
t3

units(t3$age) <- 'years'
describe(t3)

dd <- datadist(t3)
dd

# Describe distributions of variables
options(datadist='dd')
s <- summary(survived ~ age + sex + pclass + cut2(sibsp, 0:3) + cut2(parch, 0:3), data=t3)
s
plot(s, main='', subtitles=F)

#Univariable summaries of Titanic survival show that some factors affected survival rate

#We'll look at each of them (categorical or discrete quantitative)
#Use age (continuous quantitative) as independent variable
#Use probability of survival (what we're looking for) as dependent variable

#------------------------------------------------------------------------------#

#Show a 4-way relationships after collapsing levels.
#Ignore estimates with < 25 passengers (too low sample size)
tn <- transform(t3,
  agec = ifelse(age < 21, 'child', 'adult'),
  sibsp= ifelse(sibsp == 0, 'no sib/sp', 'sib/sp'),
  parch= ifelse(parch == 0, 'no par/child', 'par/child'))

g <- function(y) if(length(y) < 25) NA else mean(y)
s <- with(tn, summarize(survived, llist(agec, sex, pclass, sibsp, parch), g))

#Llist, summarize in Hmisc package

ggplot(subset(s, agec != 'NA'),
  aes(x=survived, y=pclass, shape=sex)) + 
  geom_point() + facet_grid(agec ~ sibsp * parch) + 
  xlab('Proportion Surviving') + ylab('Passenger Class') +
  scale_x_continuous(breaks=c(0, 0.5, 1))

#Multi-way summary of Titanic survival

#------------------------------------------------------------------------------#

#Explore the trends of survival with nonparametric regression
#Include Age as independent (x) and survival rate as dependent (y)
#Compare with sex, pclass, both, and neither
b <- scale_size_discrete(range=c(0.1, 0.85))

y1 <- ylab(NULL)

#Compare without sex or pclass
p1 <- ggplot(t3, aes(x=age, y=survived)) + 
      histSpikeg(survived ~ age, lowess=TRUE, data=t3) + 
      ylim(0,1) + y1
#Compare by sex
p2 <- ggplot(t3, aes(x=age, y=survived, color=sex)) + 
      histSpikeg(survived ~ age + sex, lowess=TRUE, data=t3) + 
      ylim(0,1) + y1
#Compare by pclass
p3 <- ggplot(t3, aes(x=age, y=survived, size=pclass)) + 
      histSpikeg(survived ~ age + pclass, lowess=TRUE, data=t3) + 
      b + ylim(0,1) + y1
#Compare by both sex and pclass
p4 <- ggplot(t3, aes(x=age, y=survived, color=sex, size=pclass)) + 
      histSpikeg(survived ~ age + sex + pclass, lowess=TRUE, data=t3) + 
      b + ylim(0,1) + y1
#Compare graphs together between survival rate and age
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

#Lots of analysis of data

#Compare without sex or pclass
#Strong negative linear correlation with small slope (Ages: 0-20)
#No significant correlation (Ages: 20-80)

#Compare by sex
#Strong positive linear correlation for females with small slope (All ages)
#Strong negative linear correlation for males with large slope (Ages: 0-20)
#No significant correlation for males (Ages: 20-80)
#Females had higher rates of survival compared to males for all ages

#Compare by pclass
#Strong negative correlation for 1st class with small slope (All ages)
#Strong negative correlation for 2nd class with large slope (Ages: 0-20)
#Strong negative correlation for 2nd class with small slope (Ages: 20-80)
#Strong negative correlation for 3rd class with small slope (All ages)

#Compare by both sex and pclass
#1st and 2nd class females had higher rates of survival compared to men of all classes
#1st class females had lower rates of survival compared to 2nd class females for ages 0-15
#2nd class females had steeper slope from age 40-60 compared to 1st class female
#3rd class females generally had higher survival rates compared to 2nd and 3rd class males
#3rd class females had a negative slope for ages 0-40
#3rd class females had a positive slope for ages 40-60
#------------------------------------------------------------------------------#

#Compare with number of siblings/spouses and number of parents/children

top <- theme(legend.position = 'top')
#Compare by number of siblings/spouses on board
p5 <- ggplot(t3, aes(x=age, y=survived, color=cut2(sibsp, 0:2))) + 
      stat_plsmo() + b + ylim(0, 1) + y1 + top +
      scale_color_discrete(name='siblings/spouses')
#Compare by number of parents/children on board
p6 <- ggplot(t3, aes(x=age, y=survived, color=cut2(parch, 0:2))) + 
      stat_plsmo() + b + ylim(0, 1) + y1 + top +
      scale_color_discrete(name='parents/children')

gridExtra::grid.arrange(p5, p6)

#No significant relationship when blocking siblings/spouses or parents/children 

#------------------------------------------------------------------------------#

# NOT WORKING AT THE MOMENT TRY FIXING THIS

#Fit a model that is saturated with respect to age, sex, pclass
#sibling/spouse and parent/child variations don't show complex interaction

f1 <- lrm(survived ~ sex*pclass*rcs(age,5) + 
      rcs(age,5)*(sibsp + parch), data=t3)
print(anova(f1), table.env=T, label='titanic-anova3', size='small')

# 3 way interactions
f <- lrm(survived ~ (sex + pclass + rcs(age, 5))^2 + 
     rcs(age, 5)*sibsp, data=t3)
print(f)

print(anova(f), table.env=T, label='titanic-anova3', size='small')

#------------------------------------------------------------------------------#

#Show the many effects of predictors
p <- Predict(f, age, sex, pclass, sibsp=0, fun=plogis)
ggplot(p)
ggplot(Predict(f, sibsp, age=c(10, 15, 20, 50), conf.int=F))

#Children having many siblings had lower survival.
#Married adults had slightly higher survival than unmarried adults

f <- update(f, x=T, y=T)
#x=True, y=true adds raw data to fit object so can boostrap
set.seed(131) # replicate re-samples
latex(validate(f, B=200), digits=2, size='Ssize')

#Create a Bootstrap overfitting-corrected loss nonparametric calibration curve 
cal <- calibrate(f, B=200)
plot(cal, subtitles=F)

#n = 1046
#Mean absolute error=0.011
#Mean squared error=0.00016
#Quantile of absolute error=0.018

#Little to no error but moderate problem with missing data

#------------------------------------------------------------------------------#

#Examining Missing Data Patterns
na.patterns <- naclus(data)
require(rpart) #Recursive partitioning package

who.na <- rpart(is.na(age) ~ sex + pclass + survived +
                sibsp + parch, data=data, minbucket=15)
naplot(na.patterns, 'na per var')
plot(who.na); text(who.na)
plot(na.patterns)

#Patterns of missing data


#------------------------------------------------------------------------------#

plot(summary(is.na(age) ~ sex + pclass + survived + 
             sibsp + parch, data=t3))

m <- lrm(is.na(age) ~ sex * pclass + survived + sibsp + parch, data=t3)
print(m)
print ( anova (m) , table.env = TRUE , label = 'titanic-anova.na')

#------------------------------------------------------------------------------#
#Single Conditional Mean Imputation

xtrans <- transcan(~ I(age) + sex + pclass + sibsp + parch,
                    imputed = T, pr = F, pl = F, data = t3)
summary(xtrans)

age.i <- with(t3, impute(xtrans, age, data=t3))
i <- is.imputed(age.i)
with(t3, tapply(age.i[i], list(sex[i],pclass[i]), mean))

with (t3 , tapply ( age , list ( sex , pclass ) , mean , na.rm = TRUE ))

dd <- datadist(dd, age.i)
f.si <- lrm(survived ~ (sex + pclass + rcs(age.i, 5))^2 + 
            rcs(age.i, 5)*sibsp, data=t3)
print(f.si, coefs=FALSE)

p7 <- Predict(f, age, pclass, sex, sibsp=0, fun=plogis)
p8 <- Predict(f.si, age.i, pclass, sex, sibsp=0, fun=plogis)
p <- rbind('Casewise Deletion'=p7, 'Single Imputation'=p8, rename=c(age.i='age'))

ggplot(p, groups='sex', ylab='Probability of Surviving')

print(anova(f.si))

#------------------------------------------------------------------------------#
#Multiple Imputation

#Not working at the moment

set.seed(17) #Replicate random aspects
#mi <- aregImpute(¼ age + sex + pclass + sibsp + parch + survived, data =t3 , n.impute =20 , nk =4 , pr = FALSE)
