#instrumental variables example


#install package
#install.packages("ivpack")
install.packages("AER")
#load package
#library(ivpack)
library(AER)
library(skimr)

#read dataset
#data(card.data)
load("./Crash_Course/5-IV/card.data.rda")

# IV is nearc4 (near 4 year college)
# Y outcome is lwage (log of wage)
#'A treatment' is educ (number of years of education)
# X - covariates as educ12 + exper + reg661 + reg662 + reg663 + reg664 + reg665+ reg666 + reg667 + reg668

#summary stats
mean(card.data$nearc4)
par(mfrow=c(1,2))
hist(card.data$lwage)
hist(card.data$educ)

#is the IV associated with the treatment? strenght of IV
mean(card.data$educ[card.data$nearc4==1])
mean(card.data$educ[card.data$nearc4==0])

boxplot(card.data$educ, card.data$nearc4)

#make education binary
educ12<-card.data$educ>12
#estimate proportion of 'compliers'
# to estimate the strength of the IV
# sum(educ12[card.data$nearc4==1])/n
propcomp <- mean(educ12[card.data$nearc4==1]) - mean(educ12[card.data$nearc4==0])
propcomp
# sum(educ12[card.data$nearc4==1])/length(card.data$nearc4[card.data$nearc4 == 1]) -
# sum(educ12[card.data$nearc4==0])/length(card.data$nearc4[card.data$nearc4 == 0])
# sum of all with education which are near
# minus
# all with education being far (non compliers)
# 0.12 which is not that week

#intention to treat effect
itt <- mean(card.data$lwage[card.data$nearc4==1])-
  mean(card.data$lwage[card.data$nearc4==0])
itt

#complier average causal effect
itt/propcomp


#two stage least squares

#stage 1: regress A on Z
s1<-lm(educ12~card.data$nearc4)
## get predicted value of A given Z for each subject
predtx <-predict(s1, type = "response")
table(predtx)

#stage 2: regress Y on predicted value of A
lm(card.data$lwage~predtx)

# 2SLS using AER

# https://rpubs.com/wsundstrom/t_ivreg

#2SLS using ivpack
ivmodel=ivreg(lwage ~ educ12, ~ nearc4, x=TRUE, data=card.data)
ivmodel
summary(ivmodel)
robust.se(ivmodel)


ivmodel=ivreg(lwage ~ educ12 + exper + reg661 + reg662 +
                reg663 + reg664 + reg665+ reg666 + reg667 + reg668, 
              ~ nearc4 + exper +
                reg661+ reg662 + reg663 + reg664 + reg665 + reg666 +
                reg667 + reg668, x=TRUE, data=card.data)
ivmodel
summary(ivmodel)
