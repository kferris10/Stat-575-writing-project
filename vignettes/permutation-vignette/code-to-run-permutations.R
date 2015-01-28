# SETUP -----------------------------------
library(MASS)
library(reshape2)
library(mosaic)
library(magrittr)
library(dplyr)
library(tidyr)
set.seed(42)
load("LvL_Data.RData")
source("permutation_functions.R")

# parameters -----------------------------
npoints <- 15 # number of grid points to calculate densities
B <- 10       # number of permutations

# getting p-values -----------------------
obs_diff <- dens_diff(npoints, data = pit_LvL)
pvals <- perm_pvals(B, npoints, data = pit_LvL, obs_diff = obs_diff)
save(pvals, file = "my-location.RData")

# plots ----------------------------------
library(ggplot2)
library(scales)
pvals2 <- pvals_to_long(pvals) 

# actual size
qplot(row, col, data = pvals2, colour = pval) + 
  scale_colour_gradient2(low = muted("red"), mid = "grey", high = muted("blue"), 
                         midpoint = .2)

# whether they're greater than 0.05
pvals2 <- pvals2 %>% 
  mutate(size = ifelse(pvals < .05, "mouse", "elephant") %>% 
           factor(levels = c("mouse", "elephant")))
qplot(row, col, data = pvals2, colour = size) + 
  labs(x = "Horizontal Location", y = "Vertical Location") + 
  theme_bw()

