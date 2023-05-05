##########################################################################
# CAUSAL DATA SCIENCE WITH DIRECTED ACYCLIC GRAPHS
# Online Course at Udemy.com
# Dr. Paul Hunermund
# 
# Section 4 -- Confounding Bias and Surrogate Experiments
##########################################################################

# Again, we rely on the dagitty and ggdag package
library(tidyverse)
library(dagitty)
library(ggdag)

# In this section, we will work with three new packages that we have to 
# install first using the following commands
install.packages("causaleffect")
install.packages("igraph")
install.packages("latex2exp")

library(causaleffect)
library(igraph)
library(latex2exp)

###############################
##### Backdoor adjustment #####
###############################
# Let's have a look at the DAG from the backdoor adjustment section
coords <- data.frame(matrix(c("x",0,0,
                              "y",2,0,
                              "z1",1,1,
                              "z2",0,1,
                              "z3",0,2,
                              "z4",2,2,
                              "z5",2,1,
                              "z6",1,0), nrow=8, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")

dag <- dagify(y ~ z5, y ~ z6, y ~ z1,
              z6 ~ x,
              z5 ~ z4,
              z2 ~ z3,
              z1 ~ z4, z1 ~ z3,
              x ~ z2, x ~ z1,
              exposure = "x",
              outcome = "y",
              coords=coords)

# Plot
ggdag(dag)

# Find and plot all open paths between treatment and outcome
ggdag_paths(dag)

# Find and plot all sufficient adjustment sets
ggdag_adjustment_set(dag)

# List all direct causal effects that are identifiable via backdoor adjustment
for(n in names(dag)){
   for(m in children(dag,n)){
      a <- adjustmentSets(dag, n, m, effect="direct")
      if(length(a) > 0){
         cat("The causal effect of ",n," on ",m,
             " is identifiable controlling for:\n",sep="")
         print(a, prefix=" * ")
      }
   }
}

##### Numerical Example #####

# Create background factors for nodes
e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)

# Create nodes for the DAG: y <- x, y <- z, x <- z  
z <- 1*(e_z > 0)
x <- 1*(z + e_x > 0.5)
y <- 1*(x + z + e_y > 2)

y_dox <- 1*(1 + z + e_y > 2)

df <- data.frame(x, y, z, y_dox)

# We see that P(y|do(x=1)) is not equal to P(y|x=1)
mean(y_dox)
mean(y[x==1])

# Adjustment formula: P(y|do(x=1)) = P(y|x=1, z=1)*P(z=1) + P(y|x=1, z=0)*P(z=0)
mean(y[x==1 & z==1]) * mean(z==1) + mean(y[x==1 & z==0]) * mean(z==0)

# Achive the same via inverse probability weighting
df <- df %>% group_by(z) %>% mutate(weight = mean(x))
weight <- df$weight
y_weighted <- y / weight 

# Note: inverse probability weighting formula can be written as
# n^-1 * sum_n Y*1(x==1) / weight
sum(y_weighted[x==1]) / 10000

##############################################################
##### Frontdoor adjustment and identification algorithms #####
##############################################################
# To illustrate frontdoor adjustment we switch to the R package "causaleffect", which
# uses different syntax than "dagitty" to initialize DAGs 
dag2 <- graph.formula(y +- z5, y +- z6, y +- z1,
                      z6 +- x,
                      z5 +- z4,
                      z2 +- z3,
                      z1 +- z4, z1 +- z3,
                      x +- z2, x +- z1, 
                      simplify = FALSE)

# The function causal.effect uses the algorithm by Shpitser and Pearl (2006), which
# is based on the rules of do-calculus to check whether a queried causal effect is 
# identified. If this is possible an expression for the causal effect is returned 
# (note however, that the algorithm doesn't necessarily return a minimum sufficient 
# adjustment set), otherwise and error is thrown
ce_backdoor <- causal.effect(y = "y", x = "x", z = NULL, G = dag2, expr = TRUE)
plot(TeX(ce_backdoor), cex=2)

# Now let's pretend we are not able to measure Z1. The following command sets Z1 and 
# its links with the other variables in the model to "unobserved". As we see, it's
# still possible to identify P(Y|do(X)) without accounting for Z1 via frontdoor adj.
dag3 <- set.edge.attribute(dag2, "description", c(3,10), "U")

ce_frontdoor <- causal.effect(y = "y", x = "x", z = NULL, G = dag3, expr = TRUE)
plot(TeX(ce_frontdoor), cex=3)

##### Failure to identify the causal effect #####

# Here we show an example of a simple graph for which P(Y|do(X)) is not identifiable,
# X -> Y, but there is also an unobserved confounder present that leads to a correl-
# ation (bidirected arc) between the errors of X and Y. In this case the algorithm
# throws an error

dag4 <- graph.formula(y +- x, x +- y, y +- x,  simplify = FALSE)
dag4 <- set.edge.attribute(dag4, "description", index=2:3, "U")
causal.effect(y = "y", x = "x", G = dag4)

##### Numerical Example #####

e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)
u <- rnorm(10000)

# Create nodes for the DAG: y <- z, z <- x, y <--> x  
x <- 1*(e_x + u > 0)
z <- 1*(x + e_z > 0.5)
y <- 1*(z + e_y + u > 0.5)

z_dox <- 1*(1 + e_z > 0.5)
y_dox <- 1*(z_dox + e_y + u > 0.5)

df <- data.frame(x, y, z, z_dox, y_dox)

# P(y|do(x=1))
mean(y_dox)
# P(y|x=1)
mean(y[x==1])
# Frontdoor adjustment formula
mean(y[x==1 & z==1]) * mean(x==1) * mean(z[x==1]==1) +
   mean(y[x==0 & z==1]) * mean(x==0) * mean(z[x==1]==1) +
   mean(y[x==1 & z==0]) * mean(x==1) * mean(z[x==1]==0) +
   mean(y[x==0 & z==0]) * mean(x==0) * mean(z[x==1]==0)

############################
##### Z-identification #####
############################
dag5 <- graph.formula(w -+ z, z -+ x, x -+ y, w -+ y, 
                      w -+ y, y -+ w, z -+ y, y -+ z, z -+ x, x -+ z, 
                      simplify = FALSE)
dag5 <- set.edge.attribute(dag5, "description", 5:10, "U")

# P(y|do(X)) is not identifiable in the normal graph (i.e., in non-experimental data)
causal.effect(y = "y", x = "x", G = dag5)

# P(y|do(X)) is identified in a sample in which we have intervened on Z (do(Z))
# aux.effect() function implements z-identification algorithm by Bareinboim
# and Pearl (2012)
ce_aux <- aux.effect(y = "y", x = "x", z = "z", G = dag5)
plot(TeX(ce_aux), cex=3)

##### Numerical Example #####

e_w <- rnorm(10000)
e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)
u1 <- rnorm(10000)
u2 <- rnorm(10000)
u3 <- rnorm(10000)

w <- 1*(e_w + u1 > 0)
z <- 1*(w + e_z + u2 + u3 > 0.5)
x <- 1*(z + e_x + u2 > 0.5)
y <- 1*(x + w + e_y + u1 + u3 > 1)

y_dox <- 1*(1 + w + e_y + u1 + u3 > 1)

w_doz <- 1*(e_w + u1 > 0)
x_doz <- 1*(1 + e_x + u2 > 0.5)
y_doz <- 1*(x_doz + w_doz + e_y + u1 + u3 > 1)

df <- data.frame(w, x, y, z, y_dox, w_doz, x_doz, y_doz)

# P(y|do(x=1))
mean(y_dox)
# P(y|x=1)
mean(y[x==1])
# Z-identifiability adjustment formula
mean(y_doz[w_doz==1 & x_doz==1]) * mean(w_doz==1) +
         mean(y_doz[w_doz==0 & x_doz==1]) * mean(w_doz==0)
