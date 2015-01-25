
getObsDiff <- function(npoints, data = pit_LvL) {
  # calculating densities when pitchers are responsible for men on base
  r.dens <- with(data %>% filter(resp_pit == "resp"), 
                 kde2d(px, pz, n = npoints))
  # calculating densities when pitchers are not responsible for men on base
  n.dens <- with(data %>% filter(resp_pit == "not"), 
                 kde2d(px, pz, n = npoints))
  
  # calculating observed difference in densities
  obs.diff <- n.dens$z - r.dens$z
  return(obs.diff)
}

permuteDensities <- function(B, npoints, data = pit_LvL) {
  diffs <- array(NA, dim = c(npoints, npoints, B))
  
  for(i in 1:B) {
    # permuted labels
    perm.vec <- sample(pit_LvL$resp_pit)
    
    # permuted densities
    rperm.dens <- with(pit_LvL %>% filter(perm.vec == "resp"), 
                       kde2d(px, pz, n = npoints))
    nperm.dens <- with(pit_LvL %>% filter(perm.vec == "not"), 
                       kde2d(px, pz, n = npoints))
    
    # differences
    diffs[, , i] <- nperm.dens$z - rperm.dens$z
    
  }
  
  return(diffs)
}

