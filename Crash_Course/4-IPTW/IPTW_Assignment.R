###################
#RHC Example

#load packages
library(tableone)
library(ipw)
library(sandwich) #for robust variance estimation
library(survey)


# Now load the lalonde data (which is in the MatchIt package):
  
library(MatchIt)

data(lalonde)

expit <- function(x) {1/(1+exp(-x)) }
logit <- function(p) {log(p)-log(1-p)}

#view data
View(lalonde)

# The potential confounding variables are: age, educ, black, hispan, married, nodegree, re74, re75.
black <- ifelse(as.numeric(lalonde$race) ==1, 1, 0)
hispan <- ifelse(as.numeric(lalonde$race) ==2, 1, 0)
result <- lalonde$re78

#new dataset
mydata<-cbind(lalonde$age,lalonde$educ,black,hispan,lalonde$married,lalonde$nodegree,lalonde$re74,lalonde$re75,
              lalonde$treat,
              result)
mydata<-data.frame(mydata)

xvars <- c("age","educ","black","hispan","married","nodegree","re74","re75")
col_names <- append(xvars, c("treat", "result"))

colnames(mydata) <- col_names

# Find the standardized differences for all of the confounding variables (pre-matching)
table1<- CreateTableOne(vars=xvars, strata="treat", data=mydata, test=FALSE)
print(table1,smd=TRUE)

#outcome analysis
y_trt<-mydata$result[mydata$treat==1]
y_con<-mydata$result[mydata$treat==0]

mean(y_trt) - mean(y_con)

##########################
#propensity score model
##########################

#fit a propensity score model. logistic regression

psmodel<-glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
             family=binomial(),data=mydata)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values
ps <- pscore

# 1 What are the minimum and maximum weights?  
attach(mydata)

#create weights
weight<-ifelse(treat==1,1/(ps),1/(1-ps))
min(weight)
max(weight)

# 2
# standardized differences for each
# confounder on the weighted (pseudo) population. What is the standardized
# difference for nodegree?   


#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =mydata, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

# 3
# Using IPTW, find the estimate and 95% confidence
# interval for the average causal effect. This can be obtained from svyglm

#first fit propensity score model to get weights
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age + educ + black + hispan + married + nodegree + re74 + re75,
                      data=mydata)
#numeric summary of weights
summary(weightmodel$ipw.weights)
#plot of weights
ipwplot(weights = weightmodel$ipw.weights, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$ipw.weights

#fit a marginal structural model (risk difference)
msm <- (svyglm(result ~ treat,
               design = svydesign(
                 ~ 1, weights = ~ wt,
                 data = mydata
               )))
coef(msm)
confint(msm, level = 0.95)

# 4
# Now truncate the weights at the 1st and 99th percentiles.
# fit propensity score model to get weights, but truncated
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age + educ + black + hispan + married + nodegree + re74 + re75,
                      data=mydata,
                      trunc=.01)
#numeric summary of weights
summary(weightmodel$weights.trun)
#plot of weights
ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$weights.trun

#fit a marginal structural model (risk difference)
msm <- (svyglm(result ~ treat,
               design = svydesign(
                 ~ 1, weights = ~ wt,
                 data = mydata
               )))
coef(msm)
confint(msm, level = 0.95)
