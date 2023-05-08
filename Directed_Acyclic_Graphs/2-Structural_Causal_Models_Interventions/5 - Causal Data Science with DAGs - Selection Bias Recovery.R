##########################################################################
# CAUSAL DATA SCIENCE WITH DIRECTED ACYCLIC GRAPHS
# Online Course at Udemy.com
# Dr. Paul Hunermund
# 
# Section 5 -- Recovering from Selection Bias
##########################################################################

library(causaleffect)
library(igraph)
library(latex2exp)

##### Identification #####

# Take the selection diagram from the appendix
dag <- graph.formula(w -+ s, w -+ x, w -+ z,
                     z -+ x,
                     x -+ y,
                     z -+ y, y -+ z,
                     simplify = FALSE)
# The bidirected edge between Z and Y is unobserved
dag <- set.edge.attribute(dag, "description", 6:7, "U")

# Declare node s as selection node (you can find the index
# of s in the graph by using the command "V(dag)")
dag <- set.vertex.attribute(dag, "description", 2, "S")

# Apply algorithm by Bareinboim and Tian (2015) for identification
# with selection-biased data implemented in the recover() function
ce_recover <- recover(y = "y", x = "x", G = dag)
plot(TeX(ce_recover), cex=2)

##### Numerical Example #####

# The following example shows that conditional distributions are recoverable
# from selection bias if the selection node is d-seperated from Y and thus
# confirms the result of Theorem 1 in Bareinboim and Tian (2015)
e_w <- rnorm(10000)
e_s <- rnorm(10000)
e_z <- rnorm(10000)
e_x <- rnorm(10000)
e_y <- rnorm(10000)
u1 <- rnorm(10000)

w <- 1*(e_w > 0)
s <- 1*(w + e_s > 0.5)
z <- 1*(w + e_z + u1 > 0.5)
x <- 1*(w + z + e_x > 1)
y <- 1*(x + e_y + u1 > 0.5)

y_dox <- 1*(1 + e_y + u1 > 0.5)

df <- data.frame(w, x, y, z, s, y_dox)

# P(z) != P(z|s==1), selection bias!
mean(z)
mean(z[s==1])

# But P(z|w) ~= P(z|w,s==1), because Z _||_ S | W
mean(z[w==1])
mean(z[w==1 & s==1])

# Now let's try to recover the causal effect 
mean(y_dox)
mean(y[w==1 & z==1 & x==1 & s==1]) * mean(z[w==1 & s==1]==1) +
   mean(y[w==1 & z==0 & x==1 & s==1]) * mean(z[w==1 & s==1]==0) 

# (CORRECTION: unlike in the version of the video, we should still 
# condition on either w==1 or w==0 here, thanks to Scott Mueller and 
# Kazuki Okubo for pointing this out.)