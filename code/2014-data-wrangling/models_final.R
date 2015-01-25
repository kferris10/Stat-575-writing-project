####### NOTES ########################
# 

require(dplyr)
require(pitchRx)
require(mgcv)
require(reshape2)
require(plot3D)
options(show.signif.stars = FALSE)
setwd("E:/Kevin_Ferris")
load("clean_09_13fx.Rdata")
pit_on$resp_num <- ifelse(pit_on$resp_pit == "not", 1, 0)

fitL.L <- bam(resp_num ~ te(px, pz, k = 4, bs = "ts"), 
            family = "binomial", method = "GCV.Cp", gamma = 1.2, 
            data = filter(pit_on, px > -1.2 & px < 1.2 & pz > 1 & pz < 4 & pb_hand == "L.L"))
fitL.R <- bam(resp_num ~ te(px, pz, k = 4, bs = "ts"), 
            family = "binomial", method = "GCV.Cp", gamma = 1.2, 
            data = filter(pit_on, px > -1.2 & px < 1.2 & pz > 1 & pz < 4 & pb_hand == "L.R"))
fitR.L <- bam(resp_num ~ te(px, pz, k = 4, bs = "ts"), 
            family = "binomial", method = "GCV.Cp", gamma = 1.2, 
            data = filter(pit_on, px > -1.2 & px < 1.2 & pz > 1 & pz < 4 & pb_hand == "R.L"))
fitR.R <- bam(resp_num ~ te(px, pz, k = 4, bs = "ts"), 
            family = "binomial", method = "GCV.Cp", gamma = 1.2, 
            data = filter(pit_on, px > -1.2 & px < 1.2 & pz > 1 & pz < 4 & pb_hand == "R.R"))
