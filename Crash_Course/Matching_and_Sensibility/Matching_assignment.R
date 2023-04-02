###################
#RHC Example

#install packages
install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

#load packages
library(tableone)
library(Matching)
library(MatchIt) 
library(skimr)

data(lalonde)

# The potential confounding variables are: age, educ, black, hispan, married, nodegree, re74, re75.

black <- ifelse(as.numeric(lalonde$race) ==1, 1, 0)
hispan <- ifelse(as.numeric(lalonde$race) ==2, 1, 0)

result <- lalonde$re78


#new dataset
mydata<-cbind(lalonde$age,lalonde$educ,black,hispan,lalonde$married,lalonde$nodegree,lalonde$re74,lalonde$re75,
              lalonde$treat,
              result)
mydata<-data.frame(mydata)

xvars<-c("age","educ","black","hispan","married","nodegree","re74","re75")
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

##########################
#propensity score matching
##########################

########################
# no caliper
set.seed(931139)

psmatch<-Match(Tr=mydata$treat, X = pscore ,M=1,replace=FALSE )
matched<-mydata[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

########################
# caliper

set.seed(931139)

psmatch<-Match(Tr=mydata$treat, X = pscore, M=1, replace=FALSE, caliper =0.1 )
matched<-mydata[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis
# mean of real earnings in 1978 for treated subjects minus
# the mean of real earnings in 1978 for untreated subjects? 


y_trt<-matched$result[matched$treat==1]
y_con<-matched$result[matched$treat==0]

mean(y_trt) - mean(y_con)

# Carry out a paired t-test for the effect of
# treatment on earnings.

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)
