
library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)

# Load real data to get reasonable mean and variance estimates
gF <- read.csv('/home/mgell/Work/FC/hcp/text_files/Unrelated_S900_Subject_multilist1_with_physio.csv')
gF <- gF$PMAT24_A_CR

FC <- read.csv('/home/mgell/Work/To_juseless/hcp_analysis/res_seed/samples/replic_5mm_all_mean_s2sFC.csv', header = FALSE)
upper <- upper.tri(FC)
FC <- FC[upper]

# set some parameters
# reliability -> i.e. correlation between simulated clean gF and noisy gF (with measurement error)
# this will help create an example of a simulated noisy measured data of gF
reliability <- 0.8
rho <- 0.5 # specified relationship between FC and clean gF 
n <- 1000  # how many simulated noisy datasets to make


# simulate clean data with exact relationship
# create simulated edge FC values (sim_FC) and clean gF (sim_gF) with a predefined
# strength of a relationship 
d <- rnorm_multi(n = 200, 
                 mu = c(mean(FC), mean(gF,na.rm = TRUE)),  # mean of sim FC and gF
                 sd = c(sd(FC), sd(gF,na.rm = TRUE)),      # sd of sim FC and gF
                 r = c(rho),                               # relationship between FC and gF
                 varnames = c("sim_FC", "sim_gF"),
                 empirical = TRUE)

# simulate noisy data
all_gF_noise <- matrix(0,nrow = 200,ncol = n)
all_cor <- numeric(n)

for (i in 1:n) {
  reliability_sim <- 0
  
  # now simulate a more noisy gF dataset that has a correlation reliability +-0.01 with 
  # the not noisy simulated gF
  while (abs(reliability - reliability_sim) >= 0.01) {
    sim_gF_noise <- d$sim_gF + rnorm(n = 200, mean = 0, sd = (1.1 - reliability)*10)
    reliability_sim <- cor(d$sim_gF,sim_gF_noise)
  }
  all_gF_noise[,i] <- sim_gF_noise
  all_cor[i] <- cor(d$sim_FC,sim_gF_noise)
}

# what strength of relationship can we expect between the edge FC and noisy gF given
# the specified correlation between FC and a clean measurement of gF and given
# reliability reliability?
print('actual relationship')
print(rho)
print('reliability')
print(reliability)
print('expectable relationship given noisy measurement')
print(mean(all_cor))

rho/sqrt(0.7*1) # Spearmans attenuated correlation coef 
# -> always higher but p value should be lower? not sure what the hell it shows




# 
# complement <- function(y, rho, x) {
#   if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
#   y.perp <- residuals(lm(x ~ y))
#   rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
# }
# 
# 
# 
# sim_gF_noice <- complement(y = d$sim_gF, rho = 0.8, rnorm(n = 200, mean = mean(gF,na.rm = TRUE),sd = sd(gF,na.rm = TRUE)))
# XX=scale(sim_gF_noice, center = mean(gF,na.rm = TRUE), scale = sd(gF,na.rm = TRUE))
# cor(XX,sim_gF_noice)
# 
# 
# 
# n     <- 200                   # length of vector
# rho   <- 0.8                   # desired correlation = cos(angle)
# theta <- acos(rho)             # corresponding angle
# x1    <- d$sim_gF              # fixed given data
# x2    <- rnorm(n, mean(gF,na.rm = TRUE), sd(gF,na.rm = TRUE))      # new random data
# X     <- cbind(x1, x2)         # matrix
# Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
# 
# Id   <- diag(n)                               # identity matrix
# Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
# P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
# x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
# Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
# Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
# 
# x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
# cor(x1, x)                                    # check correlation = rho
