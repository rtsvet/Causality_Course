##########################################################################
# CAUSAL DATA SCIENCE WITH DIRECTED ACYCLIC GRAPHS
# Online Course at Udemy.com
# Dr. Paul Hunermund
# 
# Section 6 -- Transportability of Causal Knowledge Across Domains
##########################################################################

library(causaleffect)
library(igraph)
library(latex2exp)

############################
##### Transportability #####
############################
dag <- graph.formula(x -+ y, 
                     z -+ x, 
                     z -+ y, 
                     s -+ z,
                     x -+ y, y -+ x, 
                     x -+ z, z -+ x,
                     simplify = FALSE)
dag <- set.edge.attribute(dag, "description", 5:8, "U")

# Use same function as in the selection bias case to declare S 
# to be selection node
dag <- set.vertex.attribute(dag, "description", 4, "S")

# transport() implements transportability algorithm by Bareinboim 
# and Pearl (2013)
ce_transport <- transport(y = "y", x = "x", D = dag)
plot(TeX(ce_transport), cex=3)

### Transportability when selection node affects a post-treatment variable
dag2 <- graph.formula(x -+ y,
                      x -+ z,
                      z -+ y,
                      x -+ y, y -+ x,
                      s -+ z,
                      simplify = FALSE)
dag2 <- set.edge.attribute(dag2, "description", 4:5, "U") 
dag2 <- set.vertex.attribute(dag2, "description", 4, "S")

ce_transport2 <- transport(y = "y", x = "x", D = dag2)
plot(TeX(ce_transport2), cex=3)

##############################
##### z-Transportability #####
##############################
dag3 <- graph.formula(x -+ y,
                      z -+ x,
                      x -+ z, z -+ x,
                      y -+ z, z -+ y,
                      s -+ z,
                      simplify = FALSE)
dag3 <- set.edge.attribute(dag3, "description", 3:6, "U") 
dag3 <- set.vertex.attribute(dag3, "description", 4, "S")

# For applying the z-transportability algorithm by Bareinboim
# and Pearl (2013), we again use the transport() function, but
# this time we specify z = "z", which tells R that we have
# data from an experiment in which Z has been manipulated

ce_transport3 <- transport(y = "y", x = "x", z = "z", D = dag3)
plot(TeX(ce_transport3), cex=3)
# Here, the x-specific causal effect is directly transportable
# if z is experimentally manipulated

#################################
##### Meta-transportability #####
#################################
# Specify selection diagram for domain 1
d1 <- graph.formula(x -+ z,
                    z -+ y,
                    x -+ y,
                    x -+ z, z -+ x,
                    x -+ y, y -+ x,
                    s1 -+ x,
                    s2 -+ y,
                    simplify = FALSE)
d1 <- set.edge.attribute(d1, "description", 4:7, "U") 
d1 <- set.vertex.attribute(d1, "description", 4:5, "S")

# P(y|do(x)) is not transportable in domain 1
transport(y = "y", x = "x", D = d1)

# Specify selection diagram for domain 2
d2 <- graph.formula(x -+ z,
                    z -+ y,
                    x -+ y,
                    x -+ z, z -+ x,
                    x -+ y, y -+ x,
                    s1 -+ x,
                    s2 -+ z,
                    simplify = FALSE)
d2 <- set.edge.attribute(d2, "description", 4:7, "U") 
d2 <- set.vertex.attribute(d2, "description", 4:5, "S")

# P(y|do(x)) is also not transportable in domain 2
transport(y = "y", x = "x", D = d2)

# Combine both selection diagrams in a list
d.comb <- list(d1, d2)

# The causal effect is meta-transportable with information
# from both source domains
m_transport <- meta.transport(y = "y", x = "x", D = d.comb)
plot(TeX(m_transport), cex=3)

#############################
##### Numerical Example #####
#############################
e1_x <- rnorm(10000)
e1_y <- rnorm(10000)
e1_z <- rnorm(10000)
u1 <- rnorm(10000)
e2_x <- rnorm(10000)
e2_y <- rnorm(10000)
e2_z <- rnorm(10000)
u2 <- rnorm(10000)

# Create nodes for source domain: y <- z, y <- x, z <- x, y <--> x, z <- s
x1 <- 1*(e1_x + u1 > 0)
z1 <- 1*(x1 + e1_z > 0.5)
y1 <- 1*(x1 + z1 + e1_y + u1 > 1)

z1_dox <- 1*(1 + e1_z > 0.5)
y1_dox <- 1*(1 + z1_dox + e1_y + u1 > 1)

# Create nodes for target domain, f_z() differs across domains!
x2 <- 1*(e2_x + u2 > 0)
z2 <- 1*(2*x2 + e2_z > 0.5)
y2 <- 1*(x2 + z2 + e2_y + u2 > 1)

z2_dox <- 1*(2*1 + e2_z > 0.5)
y2_dox <- 1*(1 + z2_dox + e2_y + u2 > 1)

# P^1(y|do(x)) != P^2(y|do(x))
mean(y1_dox)
mean(y2_dox)

# But P^2(y|do(x)) = sum_z P^1(y|do(x),z) * P^2(z|x)
mean(y1_dox[z1_dox==1]) * mean(z2[x2==1]==1) + 
   mean(y1_dox[z1_dox==0]) * mean(z2[x2==1]==0)