install.packages("tableone")
install.packages("ipw")
install.packages("sandwich")
install.packages("survey")

library(tableone)
library(ipw)
library(sandwich)
library(survey)

library(skimr)

load(url("https://biostat.app.vumc.org/wiki/pub/Main/DataSets/rhc.sav"))
#view data
View(rhc)
skim_without_charts(rhc)

#create a data set with just these variables, for simplicity
ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1

#new dataset
mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
              age,female,meanbp1,treatment,died)
mydata<-data.frame(mydata)
skim_without_charts(mydata)

##########################
#propensity score matching
##########################

#fit a propensity score model. logistic regression


psmodel <- glm(
  treatment ~ age + female + meanbp1 + ARF + CHF + Cirr + colcan + Coma + 
    lungcan + MOSF + sepsis,
    family = binomial(link = "logit"),
    data = mydata
)

#show coefficients etc
summary(psmodel)

## value of propensity score for each subject
ps <- predict(psmodel, type = "response")
# or use
#create propensity score
pscore<-psmodel$fitted.values

# create weights
weight <- ifelse(treatment == 1, 1/(ps), 1/(1-ps))

# apply weight to data
weight.and.data <- svydesign(ids = ~1, data = mydata, weights = ~ weight)

#weighted table 1
weightedtable <- svyCreateTableOne(vars = xvars, strdata = "treatment", 
                                   data = weight.and.data, test = FALSE)
