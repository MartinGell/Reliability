
library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)


# create perfectly correlated vectors with a given vector (i.e. sample cor.)
# Credits to: 
# https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables



### SET UP ###
#reliability <- c(0.85, 0.75, 0.65) # desired correlation with empirical measure
reliability <- 0.85 # desired correlation with empirical measure
n <- 100  # how many simulated noisy datasets to make

# Load real data to get reasonable mean and variance estimates
tab <- read.csv('/home/mgell/Work/FC/hcp/text_files/Unrelated_S900_Subject_multilist1_with_physio.csv')
d <- tab %>% select(Subject, Age, Gender, Strength_Unadj) #PMAT24_A_CR
d <- d %>% filter(!is.na(Strength_Unadj))

beh <- d$Strength_Unadj

noisier_beh <- function(y, rho, x) {
  y.perp <- residuals(lm(x ~ y)) # residuals of x against y (removing y from x).
  # y.prep is orthogonal to y
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

# simulate noisier data
for (rel_i in reliability) {
  
  # save
  all_noisy <- matrix(0, nrow = length(beh), ncol = n)
  all_cor <- numeric(n)
  all_means <- numeric(n)
  all_sds <- numeric(n)
  
  for (i in 1:n) {
    # create a random vector that will be manipulated to have specific cor with beh 
    x <- rnorm(n = length(beh), mean = mean(beh), sd = sd(beh))
    x <- noisier_beh(beh, reliability, x)
    #plot(new/10,beh)
    all_noisy[,i] <- x
    all_means[i] <- mean(x)
    all_sds[i] <- sd(x)
  }
  
}


# # what strength of relationship can we expect between the edge FC and noisy gF given
# # the specified correlation between FC and a clean measurement of gF and given
# # reliability reliability?
# print('actual relationship')
# print(rho)
# print('reliability')
# print(reliability)
# print('expectable relationship given noisy measurement')
# print(mean(all_cor))
# 
# rho/sqrt(0.7*1) # Spearmans attenuated correlation coef 
# # -> always higher but p value should be lower? not sure what the hell it shows
