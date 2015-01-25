
require(plyr)
require(dplyr)
require(ggplot2)

setwd("~/School/Stat 575 - Writing Project/R_Code_Data/Final_Stuff")
load("LvL_Data.RData")

p1 <- ggplot(pit_LvL, aes(x = px, y = pz)) + 
  stat_density2d(geom = "tile", contour = FALSE, 
                 aes(fill = ..density.., alpha = ..density..)) + 
  geom_segment(aes(x=-0.8, y=1.5, xend = -0.8, yend = 3.5)) + 
  geom_segment(aes(x=-0.8, y=3.5, xend = 0.8, yend = 3.5)) + 
  geom_segment(aes(x=0.8, y=3.5, xend = 0.8, yend = 1.5)) + 
  geom_segment(aes(x=0.8, y=1.5, xend = -0.8, yend = 1.5)) + 
  guides(fill = FALSE, alpha = FALSE) + theme_bw() + 
  scale_fill_gradient(low = "#FF99FF", high = "#FF0000") + 
  labs(x = "Horizontal Location", 
       y = "Vertical Location", 
       title = "Pitch Locations for LHP vs LHB")
p1 + facet_wrap(~resp_pit)



