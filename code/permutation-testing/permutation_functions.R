
# gets difference in densities
dens_diff <- function(npoints, data = pit_LvL) {
  # calculating densities when pitchers are responsible for men on base
  resp_dens <- data %>% 
    filter(resp_pit == "resp") %$% 
    kde2d(px, pz, n = npoints)
  
  # calculating densities when pitchers are not responsible for men on base
  not_dens <- data %>% 
    filter(resp_pit == "not") %$% 
    kde2d(px, pz, n = npoints)
  
  # calculating observed difference in densities
  not_dens$z - resp_dens$z
}

# calculates difference in densities for B permutations
perm_diffs <- function(B, npoints, data = pit_LvL) {
  diffs <- array(NA, dim = c(npoints, npoints, B))
  
  for(i in 1:B) {
    # permuted labels
    perm_vec <- sample(pit_LvL$resp_pit)
    
    # permuted densities
    rperm_dens <- data %>% 
      filter(perm_vec == "resp") %$% 
      kde2d(px, pz, n = npoints)
    nperm_dens <- data %>% 
      filter(perm_vec == "not") %$% 
      kde2d(px, pz, n = npoints)
    
    # differences
    diffs[, , i] <- nperm_dens$z - rperm_dens$z
    
  }
  
  diffs
}

# calulated permuted pvals
calc_pvals <- function(obs, perms) {
  # number of grid points
  npoints <- nrow(obs_diff)
  
  # obtaining p-values
  pvals <- matrix(NA, nrow = npoints, ncol = npoints) 
  for(i in 1:npoints) {
    for(j in 1:npoints) {
      dat <- data.frame(perm_vals = perms[i, j, ])
      obs_val <- obs[i, j]
      pvals[i, j] <- mosaic::pdata(abs(obs_val), 
                                   vals = abs(perm_vals), 
                                   data = dat, 
                                   lower.tail = F)
    }
  }
  pvals
}

# gets permuted pvalues
perm_pvals <- function(B, npoints, data = pit_LvL, obs_diff = NULL) {
  # observed difference
  if(!is.null(obs_diff)) obs_diff <- dens_diff(npoints, data)
  
  # permutations
  perm_dens <- perm_diffs(B, npoints, data)
  
  # obtaining p-values
  pvals <- calc_pvals(obs_diff, perm_dens)
  
  # turning into long data frame and returning
  pvals %>% 
    data.frame() %>% 
    tbl_df() %>% 
    gather(location, pval) %>% 
    mutate(col = rep(1:npoints, each = npoints), 
           row = rep(1:npoints, npoints)) %>% 
    select(row, col, pval)
}