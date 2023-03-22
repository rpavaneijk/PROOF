#### PROOF ####
## written by van Eijk RP (2023)

# Key functions to calculate PROOF statistic #

## Helper function I: set 
Set <- function (order, k,
                 no.preference = "ALL", 
                 list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP")){
  
  # Returns set of outcomes to be compared based on order
  #
  # Args:
  #. order = categorical variable containing patient preferences
  #. k = top k most important outcome variables extracted from 'order'
  #. no.preference = level in 'order' that indicates there is no preference
  #. list.preference = list to link each level of 'order' to the outcome's column name in the dataset
  
  order <- as.character (order)
  
  if (order == no.preference){
    as.character (list.preference)
  } else {
    rks <- list.preference[{strsplit (order, "")[[1]][1:k]}]
    as.character (rks)
  }
  
}

## Example code:
# Set (order = "BRFG", k = 2) # top two most important outcome variables
# Set (order = "BRFG", k = 2,
#      list.preference = list (B = "Bulbar", F = "FINE", G = "GROSS", R = "Respiratory")) # rename column names


## Helper function II: Com 
Com <- function (i, j, order = "ORDER", n.pref = 4, 
                 no.preference = "ALL",
                 list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP")){
  
  # Determines the common set of preferences for two patients (i & j)
  #
  # Args:
  #. i, j = patient data with preferences
  #. order = variable containing patient preferences
  #. n.pref = number of preferences that need to be compared
  #. no.preference = level in 'order' that indicates there is no preference
  #. list.preference = list to link each level of 'order' to the outcome's column name in the dataset
  
  # If both have no preference, compare on total
  if (i[order] == no.preference & j[order] == no.preference){
    "TOTAL"
  } else
    
    # If one patient has no preference
    if ((i[order] == no.preference & j[order] != no.preference) | (i[order] != no.preference & j[order] == no.preference) | n.pref == 1){
      "SINGLE"
    } else {
      
      # determine which levels of k are common
      (1:n.pref)[sapply (1:n.pref, function (ii){all (Set (order = i[order],
                                                           k = ii,
                                                           list.preference = list.preference,
                                                           no.preference = no.preference) %in% Set (order = j[order],
                                                                                                    k = ii,
                                                                                                    list.preference = list.preference,
                                                                                                    no.preference = no.preference))})]
    }
  
}

## Example code:
# Com (i = data.frame (ORDER = "BRGF"), j = data.frame (ORDER = "BGRF"))
# Common preferences are the first ("B"), the first 3 domains ("BRG") and the first 4 domains (i.e. all)

#. Helper function IIIa for EvalPair - Total score
EvalTotal <- function (i.total, j.total){
  
  #. Paired comparison of total score
  #. Returns 0,0.5,1
  ifelse (i.total == j.total, 0.5,
          as.numeric (i.total > j.total))
  
}

#. Helper function IIIb for EvalPair - Domains
EvalDomain <- function (i, j, vars){
  
  #. Paired comparison of domains
  #. Returns 0,0.5,1
  
  #. At least ONE worse domain?
  worse <- sum (i[, vars] < j[, vars]) > 0
  
  #. At least ONE better domain?
  better <- sum (i[, vars] > j[, vars]) > 0
  
  #. Decide winner
  if (better & !worse) {1} else {
    if (worse & !better) {0} else {0.5}
  }
  
}

## Helper function III: EvalPair 
EvalPair <- function (i, j, 
                      order = "ORDER",
                      n.pref = 4,
                      no.preference = "ALL",
                      list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP"),
                      total.only = F,
                      total.score = "TOTAL",
                      CAFS = F,
                      status.var = "STATUS",
                      stime.var = "STIME",
                      FullOutput = F){
  
  # Determines whether patient i wins from patient j
  #
  # Args:
  #. i, j = patient data with preferences and outcomes
  #. order = variable containing patient preferences
  #. n.pref = number of preferences that need to be compared
  #. no.preference = level in 'order' that indicates there is no preference
  #. list.preference = list to link each level of 'order' to the outcome's column name in the dataset
  #. total.only = default is FALSE, if TRUE all comparisons are performed on the total score
  #. total.score = column name of total score in dataset
  #. CAFS = default is FALSE, if TRUE survival time will be include in comparisons [provide as status.var & stime.var]
  #. status.var = column name of vital status in dataset
  #. stime.var = column name of survival time in dataset
  #
  # Returns:
  #. Z = 0 for loss, 1 for win, 0.5 for tie
  #. Var = variable used for final comparison
  #. Iter = number of comparisons conducted
  #. FullOutput is TRUE provides a dataframe of all comparisons that led to the final result
  
  #. Start with NULL
  outcome <- NA
  comparison <- NA
  iteration <- 0
  
  #. Check CAFS
  if (CAFS){
    
    #. Define survival variables
    status <- c (i[, status.var], j[, status.var])
    iteration <- iteration + 1
    
    #. Both are dead
    if (all (status == 1)){
      outcome <- as.numeric (i[, stime.var] > j[, stime.var])
      comparison <- "Survival time"
    } else {
      
      #. One is dead
      if (any (status == 1)){
        outcome <- as.numeric (i[, status.var] == 0)
      }
      comparison <- "Vital status" # all patients have been compared on vital
    }
  }
  
  #. This for not CAFS or if CAFS didn't result in winner
  if (is.na (outcome)){
    
    iteration <- iteration + 1
    
    #. If no patients have a preference, 
    #. Or if total score only, comparisons based on total score:
    if (({i[, order] == no.preference} & {j[, order] == no.preference}) | total.only){
      
      outcome <- EvalTotal (i.total = i[, total.score],
                            j.total = j[, total.score])
      comparison <- c (comparison[!is.na (comparison)],
                       "total score")
      
    } else {
      
      #. If both patients have a preference:
      if ({i[, order] != no.preference} & {j[, order] != no.preference}) {
        
        #. Determine number of common sets
        C <- Com (i, j, n.pref = n.pref, 
                  order = order, 
                  no.preference = no.preference, 
                  list.preference = list.preference)
        
        if ((length (C) == 0) | (length (C) == 1 & C[1] == n.pref) | (C[1] == "SINGLE")){
          
          #. If there are no common sets, or common set is equal to number of preference:
          vars <- unique (c (Set (order = i[, order], k = n.pref, 
                                  list.preference = list.preference,
                                  no.preference = no.preference),
                             Set (order = j[, order], k = n.pref,
                                  list.preference = list.preference,
                                  no.preference = no.preference)))
          
          #. Comparison
          outcome <- EvalDomain (i = i, j = j, vars = vars)
          comparison <- c (comparison[!is.na (comparison)],
                           paste (substr (vars, 1,1), collapse = ""))
          
        } else {
          
          #. If there are multiple common sets:
          outcome <- 0.5
          iteration <- iteration - 1
          domain.iteration <- 0
          
          #. Enter while loop, using domain.iteration
          while (outcome == 0.5 & length (C) > domain.iteration){
            
            iteration <- iteration + 1
            domain.iteration <- domain.iteration + 1
            
            #. Define first vars; doesn't matter if you use i or j, as C reflects common set
            vars <- Set (order = i[, order], 
                         k = C[domain.iteration],
                         list.preference = list.preference,
                         no.preference = no.preference)
            
            #. Use only current set as comparison [No Memory of previous comparison]
            if (domain.iteration > 1){
              #. Previous common set
              vars.prev <- Set (order = i[order],
                                k = C[domain.iteration-1],
                                list.preference = list.preference,
                                no.preference = no.preference)
              
              #. Exclude previous domains from current set
              vars <- vars[!vars %in% vars.prev]
            }
            
            #. Comparison
            outcome <- EvalDomain (i = i, j = j, vars = vars)
            comparison <- c (comparison[!is.na (comparison)],
                             paste (substr (vars, 1,1), collapse = ""))
            
          }
        }
        
        # Do this only if after comparison on domain there is a tie: compare on total score
        if (outcome == 0.5){
          outcome <- EvalTotal (i.total = i[, total.score],
                                j.total = j[, total.score])
          comparison <- c (comparison[!is.na (comparison)],
                           "total score")
          iteration <- iteration + 1 # This is an extra iteration
        }
        
      } else {
        
        #. For comparisons where one has no preference
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
        
        #. All comparisons in once
        all.worse <- ((i[, vars] - j[, vars]) < 0)
        all.better <- ((i[, vars] - j[, vars]) > 0)
        
        ## First element is on preferred domains, second element on remaining domains
        outcome <- ifelse ((all.better == T) & (all.worse == F), 1, 
                           ifelse ((all.worse == T) & (all.better == F), 0, 0.5))
        tie <- outcome == 0.5
        
        #. If all the above are tied, then compare on total score:
        if (sum (tie) == n.pref){
          
          # N iter = previous iterations + all domains + total score:
          iteration <- iteration + n.pref + 1 
          
          # Final comparison on total score
          outcome <- EvalTotal (i.total = i[, total.score],
                                j.total = j[, total.score])
          comparison <- c (comparison[!is.na (comparison)],
                           paste (substr (vars, 1,1)),
                           "total score")
          
        } else {
          
          #. If there is a decision, then count untill that decision:
          outcome <- outcome[min (which (tie == F))]
          iteration <- iteration + min (which (tie == F)) - 1
          comparison <- c (comparison[!is.na (comparison)],
                           paste (substr (vars[1:min (which (tie == F))], 1,1)))
        }
      }
    } 
  }
  
  #. Output
  if (FullOutput){
    return (data.frame (Outcome = c (rep (0.5, iteration-1), outcome), 
                        Variable = comparison,
                        Iteration = 1:iteration))  
  } else {
    return (data.frame (Outcome = outcome, 
                        Variable = comparison[iteration],
                        Iteration = iteration))  
  }
}

#. Example use of EvalPair ()
# i <- data.frame (TOTAL = 38,
#                  BULBAR = 12,
#                  FINE = 5,
#                  GROSS = 10,
#                  RESP = 11,
#                  ORDER = "RFBG",
#                  STIME = 12,
#                  STATUS = 0)
# j <- data.frame (TOTAL = 41,
#                  BULBAR = 11,
#                  FINE = 9,
#                  GROSS = 10,
#                  RESP = 11,
#                  ORDER = "RBFG",
#                  STIME = 12,
#                  STATUS = 0)
# 
# EvalPair (i = i, j = j,
#           CAFS = F,
#           FullOutput = T)
# # Patient i loses to j in fourth comparison on total score: first comparison "R" (tie), second "FB" (tie), third "G" (tie)
# 
# EvalPair (i = i, j = j,
#           CAFS = T,
#           FullOutput = T)
# # Adding survival time here only adds one iteration (first comparison on vital status)

PROOF <- function (data, 
                   grp.var = NULL, 
                   ctrl.cat = 0, 
                   id.var = "ID", 
                   CAFS = F,
                   order = "ORDER",
                   n.pref = 4,
                   no.preference = "ALL",
                   list.preference = list (B = "BULBAR", F = "FINE", G = "GROSS", R = "RESP"),
                   total.only = F,
                   total.score = "TOTAL",
                   status.var = "STATUS",
                   stime.var = "STIME"){
  
  # Determines each patient's rank, with or without prioritizing survival, and compares rank differences between groups
  #
  # Args:
  #. data = patient data with preferences and outcomes
  #. grp.var = name of column containing groups to compare, set to NULL to obtain patient ranks for all patients
  #. ctrl.cat = levels in 'grp.var' that is the reference
  #. id.var = name of column containing patient IDs
  #. CAFS = default is FALSE, if TRUE survival time will be include in comparisons [provide as status.var & stime.var]
  #. status.var = column name of vital status in dataset
  #. stime.var = column name of survival time in dataset
  #. order = variable containing patient preferences
  #. n.pref = number of preferences that need to be compared
  #. no.preference = level in 'order' that indicates there is no preference
  #. list.preference = list to link each level of 'order' to the outcome's column name in the dataset
  #. total.only = default is FALSE, if TRUE all comparisons are performed on the total score
  #. total.score = column name of total score in dataset
  #
  # Returns:
  #. Results = results of group comparison; only is a 'grp.var' is provided
  #. raw = the output of Eval.extended for all unique pairwise comparisons
  #. Z = matrix of all pairwise comparisons
  #. R = individual patient ranks
  
  ## Check data ##
  
  #. Required vars:
  col.nms <- if (isTRUE (CAFS)){ 
    c (id.var, as.character (list.preference), total.score, order, "STATUS", "STIME")
  } else {
    c (id.var, as.character (list.preference), total.score, order)
  }
  
  #. Colnames:
  if (all (col.nms %in% colnames (data)) == F){
    stop (paste ("The following colnames are missing:", 
                 paste0 (col.nms[(col.nms %in% colnames (data)) == F], 
                         collapse = "; ")))
  }
  
  #. Patient ids:
  if (sum (table (data[, id.var]) > 1) > 0){
    stop (paste0 ("No unique patient IDs: ",
                  paste0 ((data[, id.var])[table (data[, id.var]) > 1], 
                          collapse = "; ")))
  }
  
  #. Missing data
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
  
  #. Preferences
  if (max (nchar (data[, order][data[, order] != no.preference])) < n.pref){
    stop ("Preference string smaller than # of preferences: check args 'n.pref' & 'order' ")
  }
  
  
  ## Define data structures ##
  
  #. Overall data:
  ids <- data[, id.var]
  n <- length (ids)
  
  #. Group data
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
  
  ## Comparison matrix Z ##
  
  #. Step 1: Define the comparison matrix [All patients x All patients]
  Z <- expand.grid (c.name = ids, r.name = ids)
  Z <- matrix (paste (Z$r.name, Z$c.name, sep = ":"), ncol = n, nrow = n, byrow = T)
  
  #. Step 2: Compare only lower part of the matrix as upper part is inverse of lower
  comparisons <- Z[lower.tri (Z)]
  
  #. Parallel computing to increase speed
  comparisons <- mclapply (comparisons, function (ii){
    
    #. Define pair and dataset:
    id.pair <- str_split (ii, ":")[[1]]
    i <- data[{data[, id.var] == id.pair[1]}, ]
    j <- data[{data[, id.var] == id.pair[2]}, ]
    
    #. Pairwise comparison: i vs j
    EvalPair (i, j, 
              n.pref = n.pref, 
              total.score = total.score,
              order = order,
              no.preference = no.preference,
              total.only = total.only,
              list.preference = list.preference,
              CAFS = CAFS,
              status.var = "STATUS",
              stime.var = "STIME")
    
  }, mc.cores = 4)
  comparisons <- do.call ("rbind", comparisons)
  
  #. Step 3: Redefine Z for numeric input
  Z <- matrix (NA, ncol = n, nrow = n)
  diag (Z) <- 0.5 # Each patient is equal to itself
  colnames (Z) <- rownames (Z) <- ids
  
  # Add the comparisons to lower half of matrix
  Z[lower.tri (Z)] <- as.numeric (comparisons$Outcome)
  
  # Inverse output for upper part:
  Z1 <- t (Z)
  Z1[lower.tri (Z)] <- ifelse (Z[lower.tri (Z)] == 1, 0,
                               ifelse (Z[lower.tri (Z)] == 0, 1, 0.5))
  Z <- t (Z1)
  
  #. Step 4: Obtain patient ranks
  R <- rank (rowSums (Z))
  
  ## Group comparisons ##
  # Reference: https://en.wikipedia.org/wiki/Mann–Whitney_U_test
  # Estimation follows indirect method
  
  if (is.null (grp.var) == F){
    
    # Step 1: Calculate U-statistic [Treatment group]
    U <- sum (R[trt.idx]) - ((n.ii*(n.ii + 1))/2)
    
    # Step 2: Calculate the tied ranks & variance
    k <- unique (R[duplicated (R)])
    var <- 0
    if (length (k) != 0){
      for (kk in 1:length (k)){
        n.rk <- sum (R == k[kk]) # number of patients that share rank
        var <- var + (((n.rk^3) - n.rk)/(n*(n-1)))
      }
    }
    var <- ((n.ii*n.jj)/12) * ((n + 1) - var)
    
    # Estimate Z-statistic [with continuity correction]
    # Continuity direction towards the null by adding 0.5
    # Thus adding if smaller, substracting when bigger than mean
    mu <- ((n.ii*n.jj)/2)
    corr <- if (U < mu){0.5}else{-0.5}
    zstat <- ((U + corr) - mu) / sqrt (var) 
    pval <- 2*(1 - pnorm (abs (zstat)))
    
    # Above follows wilcox.test and provides identical output
    # wilcox.test (rowSums (Z) ~ data[, grp.var], exact = F, correct = T)
    
    ## Return results
    ## NOTE: Theta + CI are not adjusted for continuity correction
    Results <- data.frame (
      U = U,
      sigma.U = sqrt (var),
      theta = U/(n.ii*n.jj), # Win probability
      sigma.theta = sqrt (var)/(n.ii*n.jj),
      lb = (U + qnorm (0.025)*sqrt (var))/(n.ii*n.jj), # 95%CI Win probability
      ub = (U + qnorm (0.975)*sqrt (var))/(n.ii*n.jj),
      zstat = zstat,
      pval = pval,
      R.ctrl = mean (R[ctrl.idx]),
      R.trt = mean (R[trt.idx]) 
    )
    
    #. Additional metrics
    Results$R.diff <- Results$R.trt - Results$R.ctrl # Absolute difference in mean ranks
    Results$OR <- Results$theta/(1-Results$theta) # Win odds
    Results$NNT <- (2*Results$theta - 1)^-1 # Number needed to treat
    
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
    )
    )
  }
}









