##########################################################################
# CAUSAL DATA SCIENCE WITH DIRECTED ACYCLIC GRAPHS
# Online Course at Udemy.com
# Dr. Paul Hunermund
# 
# Section 3 -- Causal Discovery
##########################################################################

# To use the "pcalg" package, we need two auxilliary packages "RBGL"
# and "graph", which are not available on CRAN but on BioConductor
if (!requireNamespace("BiocManager", quietly = TRUE))
   install.packages("BiocManager")
BiocManager::install("RBGL")
BiocManager::install("graph")

install.packages("pcalg")

library(pcalg)
library(dagitty)
library(ggdag)

############################################
##### Testable implications of graphs ######
############################################
dag <- dagify(y ~ x, y ~ m, y ~ z2, y ~ z3,
              m ~ x, m ~ z1, m ~ z3,
              x ~ z1, x ~ z2,
              exposure = "x",
              outcome = "y")

# Find all d-separation relationships in a DAG
impliedConditionalIndependencies(dag)

#############################################
##### PC algorithm example calculations #####
#############################################

# Simulate data
a <- rnorm(1000)
b <- rnorm(1000)
c <- a + rnorm(1000)
d <- a + c + b + rnorm(1000)
e <- rnorm(1000)
df <- data.frame(a, b, c, d, e)

# Create a sufficient statistic for conditional independence testing.
# For Gaussian variables this is simply the variance-covariance matrix.
suffStat_gauss <- list(C = cor(df), n = nrow(df))

### Find the skelton of the graph

# Test a--b
gaussCItest(1, 2, NULL, suffStat_gauss)
gaussCItest(1, 2, c(3), suffStat_gauss)
gaussCItest(1, 2, c(3,4), suffStat_gauss)
gaussCItest(1, 2, c(3,4,5), suffStat_gauss)
# ...and so forth...
# Result: a _||_ b; remove a--b; S=empty set

# Test a--c
gaussCItest(1, 3, NULL, suffStat_gauss)
gaussCItest(1, 3, c(4), suffStat_gauss)
gaussCItest(1, 3, c(4,5), suffStat_gauss)
# ...and so forth...
# Result: no conditional independence found; keep a--c

# Test a--e
gaussCItest(1, 5, NULL, suffStat_gauss)
# ...and so forth...
# Result: a _||_ e; remove a--e; S=empty set

# Test b--e
gaussCItest(2, 5, NULL, suffStat_gauss)
# ...and so forth...
# Result: b _||_ e; remove b--e; S=empty set

# And so forth... In principle, we would need to test all the edges in the graph. 
# To save time, however, we will not go through all the steps here.

# We can autmatically find the skeleton using the "pcalg" package
skeleton <- skeleton(suffStat_gauss, indepTest = gaussCItest, labels = colnames(df), alpha = 0.01)
Rgraphviz::plot(skeleton)

### Orient the edges

# Try to orient a--d--b
gaussCItest(1, 2, NULL, suffStat_gauss)
gaussCItest(1, 2, 4, suffStat_gauss)
# Result: d is collider; a->d<-b

# Try to orient b--d--c
gaussCItest(2, 3, NULL, suffStat_gauss)
gaussCItest(2, 3, 4, suffStat_gauss)
# Result: d is collider; c->d<-b

# There are no unshielded triplets remaining, because (a--d--c--a)
# Thus, a--c can not be oriented, we thus need to record it as a<->c

### Using the implementation of the PC algorithm provided by pcalg
pc_gauss <- pc(suffStat_gauss, indepTest = gaussCItest, labels = colnames(df), alpha = 0.01)
dev.off()
Rgraphviz::plot(pc_gauss)