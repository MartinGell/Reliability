
library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)


# create perfectly correlated vectors with a given vector (i.e. sample cor.)
# Credits to: 
# https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables



### SET UP ###
#reliability <- c(0.85, 0.75, 0.65) # desired correlation with empirical measure
true_reliability <- 0.94  # actual reliability of measure
new_reliability <- c(0.9,0.85,0.8,0.75,0.7,0.65,0.6)   # desired correlation with empirical measure
n <- 1000  # how many simulated noisy datasets to make

# maximum and minimum value for behavioural measurement
# below is based on scoring and interpretation manual for NIH Toolbox
# These only make sense when using age adjusted scores => mean 100 and SD 15 SD
maximum <- 160 # 4SD 
minimum <- 30  # < 30 is motor dysfunction (more than 4SD)

outdir <- '/home/mgell/Work/Prediction/text_files/'
beh <- 'Strength_Unadj'  # or PMAT24_A_CR?
##############



# Load real data to add noise to
tab <- read.csv('/home/mgell/Work/FC/hcp/text_files/Unrelated_S900_Subject_multilist1_with_physio.csv')
d <- tab %>% select(Subject, Age, Gender, all_of(beh))
d <- d %>% filter(!is.na(d[,4]))


# Function for creating vectors with exact correlation
noisier_beh <- function(y, rho, x) {
  ### input:
  #   - y:   'real' vector we want to be correlated with x
  #   - rho: correlation/reliability between y and x
  #   - x:   random vector (this is your noise) that is optimised to correlate
  #          with y
  
  y_res <- residuals(lm(x ~ y)) # residuals of x against y (removing y from x).
  # y_res is orthogonal to y
  rho * sd(y_res) * y + y_res * sd(y) * sqrt(1 - rho^2) # optimise for specific r
}

# Function for making sure values do go past and below a value
min_max <- function(vec, min = 0, max = Inf) {
  vec[vec < min] <- min
  vec[vec > max] <- max
  return(vec)
}




# simulate noisier data
T1 <- d[,4]

for (rel_i in new_reliability) {
  
  # save
  all_noisy <- matrix(0, nrow = length(T1), ncol = n)
  all_cor <- numeric(n)
  
  for (i in 1:n) {
    # create a random vector that will be manipulated to have specific cor with beh 
    x <- rnorm(n = length(T1), mean = mean(T1), sd = sd(T1))
    T1_noisy <- noisier_beh(T1, rel_i, x)
    #plot(new/10,T1)
    T1_noisy_ok <- min_max(T1_noisy/10, min = minimum, max = maximum)
    # values end up 10*higher than original values in T1
    # when adding noise. Dividing by 10 adjusts for it. Linear scaling doesnt
    # affect correlations so this makes no difference.
    
    # Save
    all_noisy[,i] <- T1_noisy_ok
    all_cor[i] <- cor(T1_noisy_ok,T1)
  }
  
  # Check a few noisy examples
  print(cor(all_noisy[,1],T1))

  
  # correlation between T1_noisy and T1 
  print('correlation of T1_noisy and T1')
  print(mean(all_cor))
  
  write.csv(all_noisy, paste0(outdir,beh,'_wnoise_rel_',rel_i,'.csv'))
  
}



