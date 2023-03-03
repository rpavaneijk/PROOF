#### PROOF ####
# Key functions to calculate rank statistic

## Set function
Set <- function (order, k,
                 no.preference = "ALL", 
                 list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP")){
  
  ## Set of outcome indicators that are ranked as the top k most important outcome variables
  order <- as.character (order)
  
  if (order == no.preference){
    
    as.character (list.preference)
    
  } else {
    
    rks <- list.preference[{strsplit (order, "")[[1]][1:k]}]
    as.character (rks)
    
  }
}

## Example code:
# Set (order = "BRFG", 2) # two-most important outcome variables
# Set (order = "BRFG", k = 1, first.only = T) # Use only first outcome
Com <- function (i, j, n.pref = 4, order = "ORDER",
                 no.preference = "ALL",
                 list.preference = list.preference){
  
  ## Args:
  # n.pref: how many domains would you like to use
  
  # If both have all, compare on total
  if (i[order] == no.preference & j[order] == no.preference){
    "TOTAL"
  } else
    
    # If one has all, patients can be compared on all endpoints
    # This is handled in the Eval function
    if ((i[order] == no.preference & j[order] != no.preference) | (i[order] != no.preference & j[order] == no.preference) | n.pref == 1){
      "SINGLE"
    } else {
      (1:n.pref)[sapply (1:n.pref, function (ii){all (Set (order = i[order],
                                                           k = ii,
                                                           list.preference = list.preference,
                                                           no.preference = no.preference) %in% Set (order = j[order],
                                                                                                    k = ii,
                                                                                                    list.preference = list.preference,
                                                                                                    no.preference = no.preference))})]
    }
}

### Eval function:
## No memory indicated that comparisons are done irrespective of outcome on previous comparisons
Eval.expanded <- function (i, j, 
                           n.pref = 4,
                           total.score = "TOTAL",
                           order = "ORDER",
                           no.preference = "ALL",
                           total.only = F,
                           list.preference = list.preference){
  
  
  if (({i[order] == no.preference} & {j[order] == no.preference}) | total.only){
    
    ## Comparisons based on total score
    R <- ifelse (i[total.score] == j[total.score],
                 0.5,
                 as.numeric (i[total.score] > j[total.score]))
    iter <- 1
    final <- total.score
    
  } else {
    
    if ({i[order] != no.preference} & {j[order] != no.preference}) {
      
      ## Number of common sets
      C <- Com (i, j, n.pref = n.pref, 
                order = order, 
                no.preference = no.preference, 
                list.preference = list.preference)
      
      if ((length (C) == 0) | (length (C) == 1 & C[1] == n.pref) | (C[1] == "SINGLE")){
        
        # If there are no common sets, or common is equal to number of preference:
        vars <- unique (c (Set (order = i[order], k = n.pref, 
                                list.preference = list.preference,
                                no.preference = no.preference),
                           Set (order = j[order], k = n.pref,
                                list.preference = list.preference,
                                no.preference = no.preference)))
        
        ## At least ONE worse domain?
        worse <- sum (i[vars] < j[vars]) > 0
        
        ## At least ONE better domain?
        better <- sum (i[vars] > j[vars]) > 0
        
        R <-  if (isTRUE (better) & isFALSE (worse)) {1} else {
          if (isTRUE (worse) & isFALSE (better)) {0} else {0.5}
        }
        iter <- 1
        final <- paste (substr (vars, 1,1), collapse = "")
        
      } else {
        
        R <- 0.5
        iter <- 0
        while (R == 0.5 & length (C) > iter){
          
          iter <- iter + 1
          
          ## Define first vars; doesn't matter if you use i or j, as C reflects common set
          vars <- Set (order = i[order], 
                       k = C[iter],
                       list.preference = list.preference,
                       no.preference = no.preference)
          
          ## Use only current set as comparison [i.e. No Memory of previous comparison set]
          if (iter > 1){
            # Previous common set
            vars.prev <- Set (order = i[order],
                              k = C[iter-1],
                              list.preference = list.preference,
                              no.preference = no.preference)
            
            # Remove previous set from current set
            vars <- vars[!vars %in% vars.prev]
          }
          
          ## At least ONE worse domain?
          worse <- sum (i[vars] < j[vars]) > 0
          
          ## At least ONE better domain?
          better <- sum (i[vars] > j[vars]) > 0
          
          R <-  if (isTRUE (better) & isFALSE (worse)) {1} else {
            if (isTRUE (worse) & isFALSE (better)) {0} else {0.5}
          }
          final <- paste (substr (vars, 1,1), collapse = "")
          
        }
        
      }
      
      # Do this only if after comparison there is a tie: compare on total score
      if (R == 0.5){
        R <- ifelse (i[total.score] == j[total.score],
                     0.5,
                     as.numeric (i[total.score] > j[total.score]))
        iter <- iter + 1
        final <- total.score
      }
      
    } else {
      
      # For comparisons where one has no preference
      if (i[order] == no.preference){
        vars <- Set (order = j[order],
                     k = n.pref,
                     list.preference = list.preference,
                     no.preference = no.preference)
      } else {
        vars <- Set (order = i[order], 
                     k = n.pref,
                     list.preference = list.preference,
                     no.preference = no.preference)
      }
      
      ## All comparisons in once
      all.worse <- ((i[vars] - j[vars]) < 0)
      all.better <- ((i[vars] - j[vars]) > 0)
      
      ## First element is on preferred domains, second element on remaining domains
      R <- ifelse ((all.better == T) & (all.worse == F), 1, 
                   ifelse ((all.worse == T) & (all.better == F), 0, 0.5))
      tie <- R == 0.5
      
      if (sum (tie) == n.pref){
        
        # Do this only if after final comparison there is a tie: compare on total score
        R <- ifelse (i[total.score] == j[total.score],
                     0.5,
                     as.numeric (i[total.score] > j[total.score]))
        iter <- n.pref + 1
        final <- total.score
        
      } else {
        
        R <- R[min (which (tie == F))]
        iter <- min (which (tie == F))
        final <- paste (substr (vars[min (which (tie == F))], 1,1), collapse = "")
        
      }
      
    } # # # End preference comparison
    
  } # # # End total comparison
  
  return (data.frame (Z = as.numeric (R), 
                      Var = final,
                      Iter = iter)) 
}

# i <- B[sample (nrow (B), 1), c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
# j <- B[sample (nrow (B), 1), c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
# i
# j
# 
# Com (i, j, n.pref = 4, order = "ORDER", 
#      no.preference = "ALL",
#      list.preference = list.preference)
# Set ("FGRB", 4, list.preference = list.preference)

# 
# Eval.Single (i, j)
# Eval.Single (i, j, Domain = T)
# 
# R <- replicate (1000, {
#   i <- B1[sample (nrow (B1), 1), c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
#   j <- B1[sample (nrow (B1), 1), c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
#   c (Eval.Single (i, j, Domain = F, TOTAL = F)[2], Eval.Single (i, j, Domain = F)[2])
# })
# table (R[1, ], R[2, ])
# table (R[1, ])
# 
# prop.table (table (t (R)[, 1]))
# prop.table (table (t (R)[, 2]))
# prop.table (table (t (R)[, 3]))

### Final test statistic and comparisons
PROOF <- function (data, 
                   grp.var = NULL, 
                   ctrl.cat = 0, 
                   id.var = "ID", 
                   CAFS = F,
                   n.pref = 4,
                   total.score = "TOTAL",
                   order = "ORDER",
                   no.preference = "ALL",
                   list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP"),
                   total.only = F){
  
  # Required variables in data:
  ## [ ORDER; TOTAL; BULBAR; FINE; GROSS; RESP ]
  ## grp.var = NULL to just rank all patients
  
  # Required vars:
  col.nms <- if (isTRUE (CAFS)){ 
    c (id.var, as.character (list.preference), total.score, order, "STATUS", "STIME")
  } else {
    c (id.var, as.character (list.preference), total.score, order)
  }
  
  ### Check data ###
  ## Colnames:
  if (all (col.nms %in% colnames (data)) == F){
    stop (paste ("The following colnames are missing:", 
                 paste0 (col.nms[(col.nms %in% colnames (data)) == F], 
                         collapse = "; ")))
  }
  
  ## Patient ids:
  if (sum (table (data[, id.var]) > 1) > 0){
    stop (paste0 ("No unique patient IDs: ",
                  paste0 ((data[, id.var])[table (data[, id.var]) > 1], 
                          collapse = "; ")))
  }
  
  ## Missing data
  if (isTRUE (CAFS)){
    
    if (sum (is.na (data[, c ("STIME", "STATUS")])) > 0){
      stop ("Survival data missing")
    }
    
    if (sum (is.na (data[data$STATUS == 0, col.nms])) > 0){
      stop (paste0 ("Missing data detected: ", 
                    paste0 (col.nms[colSums (is.na (data[data$STATUS == 0, col.nms])) > 0],
                            collapse = "; ")))
    }
    
  } else {
    if (sum (is.na (data[, col.nms])) > 0){
      stop (paste0 ("Missing data detected: ", 
                    paste0 (col.nms[colSums (is.na (data[, col.nms])) > 0],
                            collapse = "; ")))
    }
  }
  
  ## Preferences
  if (max (nchar (data[, order][data[, order] != no.preference])) < n.pref){
    stop ("Preference string smaller than # of preferences: check args 'n.pref' & 'order' ")
  }
  
  ### Define data structures ###
  
  ## Overall data:
  ids <- data[, id.var]
  n <- length (ids)
  
  ## GRP data
  if (is.null (grp.var) == F){
    
    # Define the grp data:
    ctrl.idx <- data[, grp.var] == ctrl.cat
    trt.idx <- data[, grp.var] != ctrl.cat
    
    # Define the grp ids
    ids.ii <- data[, id.var][trt.idx]
    ids.jj <- data[, id.var][ctrl.idx]
    
    # Define the grp n
    n.ii <- length (ids.ii)
    n.jj <- length (ids.jj)
    
  }
  
  ### Calculation of Z matrix ###
  
  # Step 1: Define the comparison matrix [All patients x All patients]
  Z <- expand.grid (c.name = ids, r.name = ids) ## All possible pairs
  Z <- matrix (paste (Z$r.name, Z$c.name, sep = ":"),
               ncol = n, nrow = n, byrow = T)
  
  # Step 2: Compare only lower part of the matrix as upper part is inverse of lower
  ## Parallel computation to increase speed
  comparisons <- Z[lower.tri (Z)]
  comparisons <- mclapply (comparisons, function (ii){
    
    ## Define pair and dataset:
    id.pair <- str_split (ii, ":")[[1]]
    i <- data[{data[, id.var] == id.pair[1]}, ]
    j <- data[{data[, id.var] == id.pair[2]}, ]
    
    ## Survival
    if (isTRUE (CAFS)){
      
      if ((j$STATUS == 0) & (i$STATUS == 0)){
        
        ## Both are alive:
        
        # For each common set, who is the winner?
        Eval.expanded (i, j, 
                       n.pref = n.pref, 
                       total.score = total.score,
                       order = order,
                       no.preference = no.preference,
                       total.only = total.only,
                       list.preference = list.preference)
        
      } else {
        
        ## One or both are dead
        if ((j$STATUS == 1) & (i$STATUS == 1)){
          
          # Both are dead
          data.frame (Z = as.numeric (i$STIME > j$STIME),
                      Var = "STIME",
                      Iter = 1)
          
        } else {
          
          # One is dead
          data.frame (Z = as.numeric (i$STATUS == 0),
                      Var = "STATUS",
                      Iter = 1)
          
        }
      }
      
    } else {
      
      # If not CAFS, then:
      Eval.expanded (i, j, 
                     n.pref = n.pref, 
                     total.score = total.score,
                     order = order,
                     no.preference = no.preference,
                     total.only = total.only,
                     list.preference = list.preference)
    }
  }, mc.cores = 4)
  comparisons <- do.call ("rbind", comparisons)
  
  ## Redefine Z for numeric input
  Z <- matrix (NA, ncol = n, nrow = n)
  diag (Z) <- 0.5 ## Each patient is equal to itself
  colnames (Z) <- rownames (Z) <- ids
  
  # Add the comparisons to lower half of matrix
  Z[lower.tri (Z)] <- as.numeric (comparisons$Z)
  
  # Inverse output for upper part:
  Z1 <- t (Z)
  Z1[lower.tri (Z)] <- ifelse (Z[lower.tri (Z)] == 1, 0,
                               ifelse (Z[lower.tri (Z)] == 0, 1, 0.5))
  Z <- t (Z1)
  
  ### End calculation of Z matrix ###
  ## Reference material: https://en.wikipedia.org/wiki/Mann–Whitney_U_test
  ## Estimation follows indirect method
  
  ## Step 1: Rank patients
  R <- rank (rowSums (Z))
  
  if (is.null (grp.var) == F){
    
    ## Step 2: Calculate U-statistic [Treatment group]
    ## wilcox.test takes reference category, does not matter for inference
    U <- sum (R[trt.idx]) - ((n.ii*(n.ii + 1))/2)
    
    ## Step 3: Calculate the tied ranks & variance
    k <- unique (R[duplicated (R)])
    var <- 0
    if (length (k) != 0){
      for (kk in 1:length (k)){
        n.rk <- sum (R == k[kk]) # number of patients that share rank
        var <- var + (((n.rk^3) - n.rk)/(n*(n-1)))
      }
    }
    var <- ((n.ii*n.jj)/12) * ((n + 1) - var)
    
    ## Estimate Z-statistic [with continuity correction]
    ## Continuity direction towards the null by adding 0.5
    ## Thus adding if smaller, substracting when bigger than mean
    ## Continuity correction is helpful in smaller sample sizes
    ## Large sample sizes virtually no effect
    mu <- ((n.ii*n.jj)/2)
    corr <- if (U < mu){0.5}else{-0.5}
    zstat <- ((U + corr) - mu) / sqrt (var) 
    pval <- 2*(1 - pnorm (abs (zstat)))
    
    ## Above follows wilcox.test and provides identical output
    # wilcox.test (rowSums (Z) ~ data[, grp.var], exact = F, correct = T)
    
    ## Return results
    ## NOTE: Theta + CI are not adjusted for continuity correction
    Results <- data.frame (
      U = U,
      sigma.U = sqrt (var),
      theta = U/(n.ii*n.jj),
      sigma.theta = sqrt (var)/(n.ii*n.jj),
      lb = (U + qnorm (0.025)*sqrt (var))/(n.ii*n.jj),
      ub = (U + qnorm (0.975)*sqrt (var))/(n.ii*n.jj),
      zstat = zstat,
      pval = pval,
      R.ctrl = mean (R[ctrl.idx]),
      R.trt = mean (R[trt.idx]) 
    )
    Results$R.diff <- Results$R.trt - Results$R.ctrl
    Results$OR <- Results$theta/(1-Results$theta)
    Results$NNT <- (2*Results$theta - 1)^-1
    
    return (
      list (
        raw = comparisons,
        Z = Z,
        R = R,
        Results = Results
      )
    )
    
  } else {
    
    return (list (
      raw = comparisons,
      Z = Z,
      R = R
    ))
    
  }
  
}
