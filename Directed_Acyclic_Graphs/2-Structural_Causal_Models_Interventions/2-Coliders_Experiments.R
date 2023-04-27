##########################################################################
# CAUSAL DATA SCIENCE WITH DIRECTED ACYCLIC GRAPHS
# Online Course at Udemy.com
# Dr. Paul Hunermund
# 
# Section 2 -- Accompanying R Examples
##########################################################################

# Install and load required packages
install.packages("tidyverse")
install.packages("dagitty")
install.packages("ggdag")

library(tidyverse)
library(dagitty)
library(ggdag)

#################################
##### Collider bias example #####
#################################
# Create two independent normally distributed variables and combine
# them in a data frame
talent <- rnorm(1000)
looks <- rnorm(1000)
df <- data.frame(talent, looks)

# Compute the correlation between these two variables
df %>% summarize(correlation = cor(talent, looks))

# Create a third variable "job", which is equal to one if the sum
# of talent and looks is above the 75th percentile in the population
x <- talent + looks
job <- 1*(x > quantile(x, c(.75)))
df <- cbind(df, job)

df %>% filter(job == 1) %>% summarize(correlation = cor(talent, looks))

#############################################
### "Correlation doesn't imply causation" ###
#############################################
# In the following simulations, I will restrict attention to binary 
# variables. This has two distinct advantages. First, for binary variables
# probabilities are equal to means, so they are easy to compute in R. 
# Second, computing conditional probabilities is particularly easy with
# binary variables and the code does not become unnecessarily cluttered.

# Create background factors for nodes
e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)

# Create nodes for the DAG: y <- x, y <- z, x <- z  
z <- 1*(e_z > 0)
x <- 1*(z + e_x > 0.5)
y <- 1*(x + z + e_y > 2)
y_dox <- 1*(1 + z + e_y > 2)

# We see that P(y|do(x=1)) is not equal to P(y|x=1)
mean(y_dox)
mean(y[x==1])

#######################################
##### Visualizing DAGs with ggdag #####
#######################################
# For visualization, we will use the R package ggdag, which builds in dagitty
# More information: https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html

# Set the coordinates for the nodes in our DAG, this step is only for 
# layout purposes and can be skipped
coords <- data.frame(matrix(c("x",0,0,
                              "y",2,0,
                              "z1",1,1,
                              "z2",0,1,
                              "z3",0,2,
                              "z4",2,2,
                              "z5",2,1,
                              "z6",1,0), nrow=8, ncol=3, byrow=T))
colnames(coords) <- c("name","x","y")

# Initialize the DAG. "y ~ z5" means that z5 is a parent of y (y <- z5)
dag <- dagify(y ~ z5, y ~ z6, y ~ z1,
              z6 ~ x,
              z5 ~ z4,
              z2 ~ z3,
              z1 ~ z4, z1 ~ z3,
              x ~ z2, x ~ z1,
              exposure = "x",
              outcome = "y",
              coords=coords)

# Plot the DAG
ggdag(dag)

# Find all parents of outcome
ggdag_parents(dag, "y")

# Find and plot all open paths between treatment and outcome
ggdag_paths(dag)
