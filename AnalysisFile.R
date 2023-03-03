#### Preference II ####
# written by van Eijk RP

#### 0. Functions ####
get.Data (file = "Source/MasterSourceFile.R")
get.Data (file = "Source/Source_PROOF.R")

#### 1. Data ####
D <- get.Data (file = "Data/PROOF/DataPreferenceI.xlsx")
D1 <- get.Data (file = "Data/PROOF/DataPreferenceII.xlsx")

#### 2. Overview ####

## Select original data
B <- D[D$APROACH == "COMPLETED", ]

## Add FU data
D1$APROACH.FU <- D1$APROACH
D1$TOTAL.FU <- D1$TOTAL
D1$DATE.FU <- D1$DATE
D1$BULBAR.FU <- D1$BULBAR
D1$FINE.FU <- D1$FINE
D1$GROSS.FU <- D1$GROSS
D1$RESP.FU <- D1$RESP
D1$ORDER.FU <- D1$ORDER
B <- merge (B, D1[, c ("ID", "APROACH.FU", "DATE.FU", "TOTAL.FU",
                       "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "ORDER.FU")], all.x = T)

## Survival data
B$STATUS <- ifelse (B$STIME > quantile (B[B$STATUS == 0, ]$STIME, 0.95),
                    0, B$STATUS)
B$STIME <- ifelse (B$STIME > quantile (B[B$STATUS == 0, ]$STIME, 0.95),
                   quantile (B[B$STATUS == 0, ]$STIME, 0.95), B$STIME)

## FU status
B$FU <- ifelse (is.na (B$APROACH.FU), 
                ifelse (B$STATUS == 1, "DIED", "WITHDRAWN"),
                ifelse (B$APROACH.FU == "REFUSED", "REFUSED",
                        ifelse (is.na (B$TOTAL.FU), 
                                ifelse (B$STIME < 10, "LTFU", 
                                        "NO RESPONSE"), "COMPLETED")))

plot (survfit (Surv (STIME, STATUS) ~ DX, data = D), mark = "|", xlim = c (0, 13.5), 
      ylab = "Probability of survival", xlab = "Time since PREFERENCE I (months)", lty = c (1,2,3))
legend ("bottomleft", legend = c ("ALS", "PMA", "PLS"), lty = c (1,3,2), bty = "n")
table (B$FU)
B$FU2 <- ifelse (B$FU %in% c ("NO RESPONSE", "REFUSED", "WITHDRAWN"), "MISSING", B$FU)
table (B$FU2)
B$FU2 <- factor (B$FU2, c ("COMPLETED", "MISSING", "DIED"))

## SELECTION: ONLY PATIENTS WITH KNOWN FOLLOW-UP ##
B <- B[!B$FU == "LTFU", ]
B$PREF <- as.numeric (B$ORDER != "ALL")
B$MOST <- substr (B$ORDER, 1, 1)
B$ONSET <- factor (B$ONSET, c ("O", "B"))
B$DX <- factor (B$DX, c ("ALS", "PMA", "PLS"))

#### >> Table 1 << ####
tableone::CreateTableOne (vars = c ("AGE", "SEX", "DISDUR", "ONSET", "DX", "TOTAL", "SLOPE",
                                    "PREF", "DOMAIN", "MOST"),
                          factorVars = c ("PREF", "STATUS", "ONSET", "M"),
                          strata = "FU2",
                          data = B,
                          test = T, addOverall = T)
tapply (B$DISDUR, B$FU2, median)
kruskal.test (DISDUR ~ FU2, data = B)
IQR (B$DISDUR)

# ## Correlation between domains
# chisq.test (B$MOST, B$DOMAIN)
# polychor(B$MOST, B$DOMAIN)
# cramerV(table (B$MOST, B$DOMAIN), ci = T)

#### 3. Survival ####

## Overall survival
summary (survfit (Surv (STIME, STATUS) ~ 1, data = B), time = 12)
survdiff (Surv (STIME, STATUS) ~ DOMAIN, data = B)
survdiff (Surv (STIME, STATUS) ~ MOST, data = B)

drop1 (coxph (Surv (STIME, STATUS) ~ DOMAIN + TOTAL, data = B), test = "Chisq")
drop1 (coxph (Surv (STIME, STATUS) ~ MOST + TOTAL, data = B), test = "Chisq")

## Figure
km_domain <- survfit (Surv (STIME, STATUS) ~ DOMAIN, se = F, data = B)
km_most <- survfit (Surv (STIME, STATUS) ~ MOST, data = B)

## Table at risk table
time <- c (0,3,6,9,12,15)
evts_domain <- At.Risk (data = B, grp.var = "DOMAIN", time = time, clean = T)
evts_most <- At.Risk (data = B, grp.var = "MOST", time = time, clean = T)

# Code figure in Fig2_v2.R

#### 4. PROOF ####

## Expanded function [See Source_PROOF.R]
# Allow comparisons on either 1-4 levels of preference; other ordering variables
R_v1 <- PROOF (B, n.pref = 1)
R_v4 <- PROOF (B, n.pref = 4)
R_vDomain <- PROOF (B, n.pref = 1, order = "DOMAIN")
R_vTotal <- PROOF (B, total.only = T)

## Verification:
plot (rank (B$TOTAL), R_vTotal$R)
plot (R_v4$R, R_v1$R)

plot (data.frame (R_v1 = R_v1$R,
                  R_v4 = R_v4$R,
                  R_vDomain = R_vDomain$R,
                  R_vTotal = R_vTotal$R))
cor (data.frame (R_v1 = R_v1$R,
                 R_v4 = R_v4$R,
                 R_vDomain = R_vDomain$R,
                 R_vTotal = R_vTotal$R), method = "spearman")

## Unique comparisons:
(nrow (B)^2 - nrow (B))/2
nrow (R_v4$raw)
n <- seq (50, 500, by = 10)
plot (n, ((n^2 - n)/2), type = "l", xlab = "Sample size", ylab = "Number of unique comparisons")

## Number of comparisons based on FRS on first comparison [both pairs had 'ALL']
table (R_v4$raw$Var == "TOTAL" & R_v4$raw$Iter == 1)/((nrow (B)^2 - nrow (B))/2)
(table (B$ORDER == "ALL")[2] / nrow (B)) * ((table (B$ORDER == "ALL")[2] - 1) / (nrow (B)-1))

table (R_v4$raw$Var == "TOTAL" & R_v4$raw$Iter == 2) # number of comparison that resulted in tie
table (R_v1$raw$Var == "TOTAL" & R_v1$raw$Iter == 2) # additional comparisons helped to break tie:
# Reducing number from 16916 to 9789


#### 5. Missing data ####

## Data in progeny found for one patients in window [+/- 45 days around median][i.e. FRS data, not survival]
B[B$ID == "ALS35500", ]$DATE.FU <- as.character ("2022-05-02")
B[B$ID == "ALS35500", c ("TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")] <- c (28, 12, 0, 5, 11)

## Imputed model [Survivors only]:
B$l.DISDUR <- log (B$DISDUR)
B$e.SLOPE <- exp (B$SLOPE)
B$l.DXDELAY <- log (abs (B$DXDELAY))
B$NELSON <- nelsonaalen(B, STIME, STATUS)

set.seed (28022023) 
M <- aregImpute (~ I(NELSON) + STATUS +
                   PREF + DOMAIN + l.DISDUR + I(K) + I(M) + e.SLOPE +
                   SEX + AGE + BULBAR + ONSET + l.DXDELAY +
                   FINE + GROSS + I(RESP) + DX +
                   RILUSE + BULBAR.FU + FINE.FU + GROSS.FU + I(RESP.FU) +
                   e.SLOPE:DX,
                 nk = 4,
                 data = B, n.impute = 100,
                 burnin = 100, B = 2000) 

CompleteData <- function (data, M){
  
  ## Complete variables
  for (var.ii in c ("BULBAR", "FINE", "GROSS", "RESP")){
    data[is.na (data[,paste0 (var.ii, ".FU")]), paste0 (var.ii, ".FU")] <-
      round (rowMeans (M$imputed[[paste0 (var.ii, ".FU")]]))
  }
  
  ## Recalculate variables:
  data$TOTAL.FU <- rowSums (data[, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")])
  data$TOTAL.CFB <- data$TOTAL.FU - data$TOTAL
  data$BULBAR.CFB <- data$BULBAR.FU - data$BULBAR
  data$FINE.CFB <- data$FINE.FU - data$FINE
  data$GROSS.CFB <- data$GROSS.FU - data$GROSS
  data$RESP.CFB <- data$RESP.FU - data$RESP
  
  data[data$STATUS == 1, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", 
                            "TOTAL.FU", "TOTAL.CFB",
                            "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB")] <- NA
  
  ## Return data
  return (data)
  
}

## Complete dataset:
B.imp <- CompleteData (data = B, M)
plot (B$TOTAL, B$TOTAL.FU, ylab = "Follow-up", xlab = "Baseline")
points (B.imp$TOTAL, B.imp$TOTAL.FU, col = is.na (B$TOTAL.FU) + 1)
legend ("topleft", legend = c ("Observed", "Imputed"), col = 1:2, pch = 1)


#### 6. CAFS ####
CAFS <- PROOF (B.imp, n.pref = 1,
               CAFS = T,
               total.score = "TOTAL.CFB",
               total.only = T)
RKS <- PROOF (B)

## Validation:
plot (B$STIME[B$STATUS == 1], CAFS$R[B$STATUS == 1])
plot (B.imp$TOTAL.CFB[B$STATUS == 0], CAFS$R[B$STATUS == 0])

## Follow-up PROOF ranks
RKS.CFB <- PROOF (B.imp,
                  CAFS = T,
                  total.score = "TOTAL.CFB",
                  list.preference = list (
                    B = "BULBAR.CFB",
                    F = "FINE.CFB",
                    G = "GROSS.CFB",
                    R = "RESP.CFB"
                  ))
RKS.FU <- PROOF (B.imp,
                 CAFS = T,
                 total.score = "TOTAL.FU",
                 list.preference = list (
                   B = "BULBAR.FU",
                   F = "FINE.FU",
                   G = "GROSS.FU",
                   R = "RESP.FU"
                 ))

## Create figure
brks <- quantile (1:430)
brks.fu <- c (1, sum (B$STATUS) + quantile (1:329))
base <- factor (cut (rank (RKS$R), breaks = brks, include.lowest = T), 
                labels = c ("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))

t_12 <- factor (cut (RKS.CFB$R, breaks = brks.fu, include.lowest = T),
                labels = c ("Ranks\n1-101",
                            "Ranks\n102-184", 
                            "Ranks\n185-266",
                            "Ranks\n267-348",
                            "Ranks\n349-430"))
tab.imp.cfb <- table (base, t_12)

t_12 <- factor (cut (RKS.FU$R, breaks = brks.fu, include.lowest = T),
                labels = c ("Ranks\n1-101",
                            "Ranks\n102-184", 
                            "Ranks\n185-266",
                            "Ranks\n267-348",
                            "Ranks\n349-430"))
tab.imp.fu <- table (base, t_12)

## Results table:
chisq.test (table (base, B$STATUS))
prop.table (table (base, B$STATUS), 1)

cor.test (RKS$R, RKS.FU$R)
cor.test (RKS$R, RKS.CFB$R)


#### 7. Simulation ####

for (i in 2000:2023){
  set.seed (2003)
  B.sim_trt <- B.imp[sample (nrow (B.imp), size = 50, replace = T), ]
  B.sim_con <- B.imp[sample (nrow (B.imp), size = 50, replace = T), ]
  B.sim_trt$TRT <- 1; B.sim_con$TRT <- 0
  
  B.sim_trt$BULBAR.FU <- B.sim_trt$BULBAR.FU + 1 
  B.sim_trt$BULBAR.FU <- B.sim_trt$FINE.FU + 1 
  B.sim_trt$BULBAR.FU <- B.sim_trt$GROSS.FU + 1 
  B.sim_trt$BULBAR.FU <- B.sim_trt$RESP.FU + 1 
  
  B.sim <- rbind (B.sim_trt,
                  B.sim_con)
  B.sim$ID <- 1:nrow (B.sim)
  
  plot (survfit (Surv (STIME, STATUS) ~ TRT, data = B.sim), col = 1:2, main = i)
  coef (coxph (Surv (STIME, STATUS) ~ TRT, data = B.sim))
}


trt.effect <- function (B.sim){
  
  t.bulb <- t.test (B.sim$BULBAR.FU ~ B.sim$TRT == 0)
  t.fine <- t.test (B.sim$FINE.FU ~ B.sim$TRT == 0)
  t.gross <- t.test (B.sim$GROSS.FU ~ B.sim$TRT == 0)
  t.resp <- t.test (B.sim$RESP.FU ~ B.sim$TRT == 0)
  t.total <- t.test (B.sim$TOTAL.FU ~ B.sim$TRT == 0)
  
  data.frame (Outcome = c ("Bulbar", "Fine", "Gross", "Resp", "Total"),
              Mean_Treatment = c (round (t.bulb$estimate[1], 1),
                                round (t.fine$estimate[1], 1),
                                round (t.gross$estimate[1], 1),
                                round (t.resp$estimate[1], 1),
                                round (t.total$estimate[1], 1)),
              Mean_Control = c (round (t.bulb$estimate[2], 1),
                                  round (t.fine$estimate[2], 1),
                                  round (t.gross$estimate[2], 1),
                                  round (t.resp$estimate[2], 1),
                                  round (t.total$estimate[2], 1)),
              Mean_difference = c (round (diff (-t.bulb$estimate), 1),
                                   round (diff (-t.fine$estimate), 1),
                                   round (diff (-t.gross$estimate), 1),
                                   round (diff (-t.resp$estimate), 1),
                                   round (diff (-t.total$estimate), 1)),
              CI = c (paste (round (t.bulb$conf.int,1), collapse = " - "),
                      paste (round (t.fine$conf.int,1), collapse = " - "),
                      paste (round (t.gross$conf.int,1), collapse = " - "),
                      paste (round (t.resp$conf.int,1), collapse = " - "),
                      paste (round (t.total$conf.int,1), collapse = " - ")
                      ),
              pval = c (round (t.bulb$p.value, 3),
                        round (t.fine$p.value, 3),
                        round (t.gross$p.value, 3),
                        round (t.resp$p.value, 3),
                        round (t.total$p.value, 3)
                        ))
  
}


plot (survfit (Surv (STIME, STATUS) ~ TRT, data = B.sim), mark = "|",
      col = 1:2)
coxph (Surv (STIME, STATUS) ~ TRT, data = B.sim)


trt.effect (B.sim)
table (B.sim$MOST)
table (B.sim$DOMAIN)

CAFS <- PROOF (B.sim, n.pref = 1,
               CAFS = T,
               total.score = "TOTAL.FU",
               total.only = T,
               grp.var = "TRT")
wilcox.test(CAFS$R ~ B.sim$TRT)
boxplot (CAFS$R ~ B.sim$TRT)


rks <- PROOF (B.sim, n.pref = 1,
       order = "ORDER",
       CAFS = T,
       total.score = "TOTAL.FU",
       total.only = F,
       list.preference = list (
         B = "BULBAR.FU",
         F = "FINE.FU",
         G = "GROSS.FU",
         R = "RESP.FU"),
       grp.var = "TRT")
boxplot (rks$R ~ B.sim$TRT)
CAFS$Results




430 - sum (B$STATUS)

sum (B$STATUS) + quantile (1:329)

quantile (1:430, seq (0,1, by = 1/11))



B.imp <- CompleteData (data = B, M, imp = 1)

CAFS$R
CAFS$R
brks <- quantile (1:430)
brks.fu <- c (1, sum (B$STATUS) + quantile (1:329))

B.imp$R.TOTAL.f <- factor (cut (rank (B.imp$TOTAL), breaks = brks, include.lowest = T), 
                           labels = c ("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))
B.imp$R.TOTAL.FU.f <- factor (cut (Rank.Var[, 1], breaks = brks.fu, include.lowest = T),
                              labels = c ("Ranks\n1-101",
                                          "Ranks\n102-184", 
                                          "Ranks\n185-266",
                                          "Ranks\n267-348",
                                          "Ranks\n349-430"))

plt <- expand.grid (T1 = levels (B.imp$R.TOTAL.f), 
                    T2 = levels (B.imp$R.TOTAL.FU.f))

tab <- table (B.imp$R.TOTAL.f, B.imp$R.TOTAL.FU.f)











plot (Rank.Var[,1], Rank.Var[,3])

plot (Rank.Var[1, ], type = "l", ylim = c (1, nrow (B)))
for (i in 32:41){
  lines (Rank.Var[i, ], col = i)
  abline (h = mean (Rank.Var[i, ]), col = i)
}

plot (Rank.Var[1, ], Rank.Var[3, ])


plot (apply (Rank.Var,1,sd), rowMeans (sapply (1:100, function (i){CompleteData (data = B, M, imp = 1)$TOTAL.CFB})))



## Define new dataset:
I <- B[, c ("ID", "FU", "DISDUR", "DONSET",
            "BULBAR", "FINE", "GROSS", "RESP", "TOTAL",
            "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "TOTAL.FU",
            "ORDER", "STIME", "STATUS")]

I[is.na (I$BULBAR.FU), ]$BULBAR.FU <- round (rowMeans (M$imputed$BULBAR.FU))
I[is.na (I$FINE.FU), ]$FINE.FU <- round (rowMeans (M$imputed$FINE.FU))
I[is.na (I$GROSS.FU), ]$GROSS.FU <- round (rowMeans (M$imputed$GROSS.FU))
I[is.na (I$RESP.FU), ]$RESP.FU <- round (rowMeans (M$imputed$RESP.FU))
I$TOTAL.FU <- rowSums (I[, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")])
I[I$STATUS == 1, c ( "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "TOTAL.FU")] <- NA

## Imputed values:
plot (I$TOTAL, I$TOTAL.FU, col = as.numeric (I$FU %in% c ("DIED", "COMPLETED") == F) + 1,
      ylab = "Month 12 score", xlab = "Baseline score")
abline (0,1)










table (R_v1$Z)
table (R_v4$Z)
table (R_vTotal$Z)
table (R_vDomain$Z)

head (R_v1$raw)

R_v1_fu <- PROOF (B, n.pref = 1,
                  list.preference = list (B = "I1", 
                                          F = "I2", 
                                          G = "I3",
                                          R = "RESP"))
table (R_v1$Z)
table (R_v1a$Z)


X <- replicate (1e5, {
  
  s <- sample (nrow (B), 2, replace = F)
  i <- B[s[1], c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
  j <- B[s[2], c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")]
  
  c (Eval.expanded (i, j, n.pref = 1, full.output = T, order = "ORDER"),
     Eval.expanded (i, j, n.pref = 2, full.output = T, order = "ORDER"),
     Eval.expanded (i, j, n.pref = 3, full.output = T, order = "ORDER"),
     Eval.expanded (i, j, n.pref = 4, full.output = T, order = "ORDER"), 
     Eval.expanded (i, j, n.pref = 4, full.output = T, order = "ORDER", total.only = T))
  
})

X <- as.data.frame (t (X))
names (X) <- paste0 (names (X), "_", rep (1:5, each = 3))
head (X)

table (as.numeric (X$Z_1))
table (as.numeric (X$Z_2))
table (as.numeric (X$Z_3))
table (as.numeric (X$Z_4))
table (as.numeric (X$Z_5))

table (as.character (X$Var_1), as.character (X$Iter_1))["TOTAL", ]
table (as.character (X$Var_2), as.character (X$Iter_2))["TOTAL", ]
sum (table (as.character (X$Var_3), as.character (X$Iter_3))["TOTAL", ])
sum (table (as.character (X$Var_4), as.character (X$Iter_4))["TOTAL", ])

## Weight of variable
table (as.character (X$Var_4)[as.numeric (X$Z_4) == 0.5], 
       as.numeric (X$Iter_4)[as.numeric (X$Z_4) == 0.5])

sum (as.character (X$Var_4) == "TOTAL")/nrow (X)
sapply (c ("B", "F", "G", "R"), function (domain){
  sum (ifelse (as.character (X$Var_4) == "TOTAL", 0, as.numeric (str_detect (X$Var_4, domain)) * (1/nchar (X$Var_4))))
})/nrow (X)
table (B$MOST)/nrow (B)

table (as.numeric (X$Iter_4),
       as.numeric (X$Z_4))

as.numeric (X$Z_4)[as.numeric (X$Iter_4) == 1]


round (100000 * ((table (B$MOST)/nrow (B))[1] * ((table (B$MOST)[2:5])/(nrow (B)-1)) + 
                   ((table (B$MOST)[2:5])/nrow (B)) * (table (B$MOST)/(nrow (B)-1))[1] +
                   ((table (B$MOST)/nrow (B)) * ((table (B$MOST) - 1)/(nrow (B)-1)))[2:5]))
round (100000 * (table (B$MOST)/nrow (B)) * ((table (B$MOST) - 1)/(nrow (B)-1)))

## Probability of obtaining a tie:
dms <- c ("BULBAR", "FINE", "GROSS", "RESP", "TOTAL")
sapply (dms, function (domain){
  sum ((table (B[,domain])/nrow (B)) * ((table (B[,domain]) - 1)/ (nrow (B) - 1)))
})
# sum (replicate (1e6, {diff (sample (B$BULBAR, 2)) == 0}))/1e6 # validation of the above

PREF <- seq (0, 1, by  = 0.01)

plot (PREF, PREF * (((nrow (B)*PREF)-1)/(nrow (B)-1)), type = "l",
      xlab = "Having no preference", ylab = "Proportion of comparisons based on total score")
abline (v = 162/nrow (B), lty = 3)








sum ((table (B$RESP)/nrow (B)) * ((table (B$RESP)-1)/ (nrow (B) -1)))


table (as.character (X$Iter_1))
table (as.character (X$Iter_2))
table (as.character (X$Iter_3))
table (as.character (X$Iter_4))




## Calculate PROOF at baseline & Z-matrix:
B <- PROOF (B, rank = T)
B1 <- PROOF (B, rank = T, Single = T)
Z <- PROOF (B, rank = T, return.matrix = T)









# 
# ## Restricted mean survival time [NOT IN MANUSCRIPT]
# # # Reference paper: On comparison of two classification methods with survival endpoints; Y Lu, H Jin, J Mi
# t <- 12
# m <- survival:::survmean (km_overall, rmean = t)$matrix["rmean"]
# mi_frs <- as.numeric (survival:::survmean (km_frs, rmean = t)$matrix[, "rmean"])
# mi_proof <- as.numeric (survival:::survmean (km_proof, rmean = t)$matrix[, "rmean"])
# mi_domain <- as.numeric (survival:::survmean (km_domain, rmean = t)$matrix[, "rmean"])
# pi_proof <- prop.table (table (B$QTL))
# pi_frs <- prop.table (table (B$QTL.TOTAL))
# pi_domain <- prop.table (table (cut (B1$R, brks, include.lowest = T)))
# 
# DOS_frs <- sum (((mi_frs - m)^2) * pi_frs)
# DOS_proof <- sum (((mi_proof - m)^2) * pi_proof)
# DOS_domain <- sum (((mi_domain - m)^2) * pi_domain)

# par (mfrow = c (1,2))
# plot (mi_frs, ylim = c (6, 12), type = "b", ylab = "RMST Total Score", xlab = "Quartile")
# #points (mi_proof, type = "b", col = 3)
# #legend ("bottomright", legend = c ("ALSFRS", "PROOF"), pch = 16, col = c (1,3))
# abline (h = m, lty = 1)
# segments (x0 = 1:4,
#           x1 = 1:4,
#           y0 = mi_frs,
#           y1 = m, lty = 3)
# 
# plot (mi_proof, ylim = c (6, 12), type = "b", ylab = "RMST PROOF", xlab = "Quartile", col = 3)
# #points (mi_proof, type = "b", col = 3)
# #legend ("bottomright", legend = c ("ALSFRS", "PROOF"), pch = 16, col = c (1,3))
# abline (h = m, lty = 1)
# segments (x0 = 1:10,
#           x1 = 1:10,
#           y0 = mi_proof,
#           y1 = m, lrt = 3, col = 3)





## Bootstrapped added value:
summary (coxph (Surv (STIME, STATUS) ~ TOTAL, data = B))$concordance[1]
summary (coxph (Surv (STIME, STATUS) ~ TOTAL + MOST, data = B))$concordance[1]
summary (coxph (Surv (STIME, STATUS) ~ TOTAL + DOMAIN, data = B))$concordance[1]

summary (coxph (Surv (STIME, STATUS) ~ TOTAL, data = B))$concordance[1]
drop1 (coxph (Surv (STIME, STATUS) ~ TOTAL + MOST, data = B), test = "Chisq")
drop1 (coxph (Surv (STIME, STATUS) ~ TOTAL + DOMAIN, data = B), test = "Chisq")


set.seed (22022023)
Boot <- replicate (1e4, {
  
  B.boot <- B[sample (nrow (B), replace = T), ]
  c (summary (coxph (Surv (STIME, STATUS) ~ TOTAL, data = B.boot))$concordance[1],
     summary (coxph (Surv (STIME, STATUS) ~ TOTAL + MOST, data = B.boot))$concordance[1],
     summary (coxph (Surv (STIME, STATUS) ~ TOTAL + DOMAIN, data = B.boot))$concordance[1])
  
})
quantile (t (Boot)[, 2] - t (Boot)[, 1], c (0.025, 0.975))
mean (t (Boot)[, 2] - t (Boot)[, 1])


## Calculate PROOF at baseline & Z-matrix:
B <- PROOF (B, rank = T)
B1 <- PROOF (B, rank = T, Single = T)
Z <- PROOF (B, rank = T, return.matrix = T)

# #### Non-positivity ####
# theta_original <- PROOF (B,
#        grp.var = "ONSET", 
#        ctrl.cat = "B")
# 
# # Theta: 0.5794868
# sum (Z[B[B$ONSET == "O", ]$ID, B[B$ONSET == "B", ]$ID]) / prod (table (B$ONSET))
# p_plus <- sum (Z[B[B$ONSET == "O", ]$ID, B[B$ONSET == "B", ]$ID] == 1)/prod (table (B$ONSET))
# p_null <- sum (Z[B[B$ONSET == "O", ]$ID, B[B$ONSET == "B", ]$ID] == 0.5)/prod (table (B$ONSET))
# p_neg <- sum (Z[B[B$ONSET == "O", ]$ID, B[B$ONSET == "B", ]$ID] == 0)/prod (table (B$ONSET))
# theta <- p_plus + 0.5*p_null
# win_odds <- theta / (1 - theta)
# win_ratio <- p_plus/p_neg
# theta_original$theta / (1 - theta_original$theta)
# 
# ## Similar to Brunner (2021):
# 0.5 + ((mean (rank (rowSums (Z))[B$ONSET == "O"]) - mean (rank (rowSums (Z))[B$ONSET == "B"])) / nrow (B))

## Small intermezzo: intransivity shown
# B1 <- B[sample (nrow (B), size = 50), ]
# B1$ORDER <- "ALL"
# Z <- PROOF (B1, rank = T, return.matrix = T)
# R <- PROOF (B1, grp.var = "SEX", ctrl.cat = "F")
# sum (Z[B1[B1$SEX == "M", ]$ID, B1[B1$SEX == "F", ]$ID])
# 
# X <- replicate (100, {
#   
#   B1 <- B[!B$ORDER == "ALL", ]
#   B1 <- B[sample (nrow (B), size = 50), ]
#   B1$SEX <- sample (rep (c ("F", "M"), each = 25))
#   Z <- PROOF (B1, rank = T, return.matrix = T)
#   R <- PROOF (B1, grp.var = "SEX", ctrl.cat = "M")
#   c (sum (Z[B1[B1$SEX == "M", ]$ID, B1[B1$SEX == "F", ]$ID]), R$U, prod (table (B1$SEX)))
#   
# })
# plot (X[1, ]/X[3, ], X[2, ]/X[3, ])
# abline (0,1)
# abline (h = 0.5, v = 0.5)
# range (X[1, ]/X[3, ])
# range (X[2, ]/X[3, ])


## Group by ranks
brks <- quantile (1:nrow (B), seq (0,1, by = 0.25))
B$QTL <- cut (B$R, brks, include.lowest = T)
B$QTL.TOTAL <- cut (rank (B$TOTAL), brks, include.lowest = T)
B$FT9 <- rowSums (B[, c ("BULBAR", "FINE", "GROSS", "RESP")] <= 9)

# tableone::CreateTableOne (vars = c ("AGE", "SEX", "DISDUR", "ONSET", "DX", "TOTAL", "SLOPE",
#                                     "PREF", "STATUS", "M"),
#                           factorVars = c ("PREF", "STATUS", "ONSET", "M"),
#                           strata = "QTL",
#                           data = B,
#                           test = T, addOverall = T)
# tapply (B$DISDUR, B$QTL, IQR)
# kruskal.test (DISDUR ~ QTL, data = B)
# IQR (B$DISDUR)



#### 4. Missing ####

## Data in progeny found for one patients in window [+/- 45 days around median][i.e. FRS data, not survival]
B[B$ID == "ALS35500", ]$DATE.FU <- as.character ("2022-05-02")
B[B$ID == "ALS35500", c ("TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")] <- c (28, 12, 0, 5, 11)

## Imputed model:
B$NELSON <- nelsonaalen(B, timevar = STIME, statusvar = STATUS)
B$l.DISDUR <- log (B$DISDUR)
B$e.SLOPE <- exp (B$SLOPE)
B$l.DXDELAY <- log (abs (B$DXDELAY))

set.seed (31102022) 
M <- aregImpute (~ PREF + STATUS + I(NELSON) +
                   l.DISDUR + I(K) + I(M) + e.SLOPE +
                   SEX + AGE + BULBAR + ONSET + l.DXDELAY +
                   FINE + GROSS + I(RESP) + DX +
                   RILUSE + BULBAR.FU + FINE.FU + GROSS.FU + I(RESP.FU),
                 type = "regression", curtail = T,
                 tlinear = T, nk = 4,
                 boot.method = "approximate bayesian", 
                 data = B, n.impute = 100,
                 burnin = 100, B = 2000) 

## Define new dataset:
I <- B[, c ("ID", "FU", "DISDUR", "DONSET",
            "BULBAR", "FINE", "GROSS", "RESP", "TOTAL",
            "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "TOTAL.FU",
            "ORDER", "STIME", "STATUS")]

I[is.na (I$BULBAR.FU), ]$BULBAR.FU <- round (rowMeans (M$imputed$BULBAR.FU))
I[is.na (I$FINE.FU), ]$FINE.FU <- round (rowMeans (M$imputed$FINE.FU))
I[is.na (I$GROSS.FU), ]$GROSS.FU <- round (rowMeans (M$imputed$GROSS.FU))
I[is.na (I$RESP.FU), ]$RESP.FU <- round (rowMeans (M$imputed$RESP.FU))
I$TOTAL.FU <- rowSums (I[, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")])
I[I$STATUS == 1, c ( "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "TOTAL.FU")] <- NA

## Imputed values:
plot (I$TOTAL, I$TOTAL.FU, col = as.numeric (I$FU %in% c ("DIED", "COMPLETED") == F) + 1,
      ylab = "Month 12 score", xlab = "Baseline score")
abline (0,1)

## Change from baseline
I$TOTAL.CFB <- I$TOTAL.FU - I$TOTAL
I$BULBAR.CFB <- I$BULBAR.FU - I$BULBAR
I$FINE.CFB <- I$FINE.FU - I$FINE
I$GROSS.CFB <- I$GROSS.FU - I$GROSS
I$RESP.CFB <- I$RESP.FU - I$RESP

## Observed
I.obs <- I[, c ("ID", "TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "ORDER", "STIME", "STATUS")]
I.obs <- rn (I.obs, 
             old = c ("TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU"),
             new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))

## CFB
I.cfb <- I[, c ("ID", "TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB", "ORDER", "STIME", "STATUS")]
I.cfb <- rn (I.cfb, 
             old = c ("TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB"),
             new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))

## CAFS
I.cafs <- I[, c ("ID", "TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB", "ORDER", "STIME", "STATUS")]
I.cafs <- rn (I.cafs, 
              old = c ("TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB"),
              new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))
I.cafs$ORDER <- "ALL"

I.total <- I[, c ("ID", "TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU", "ORDER", "STIME", "STATUS")]
I.total <- rn (I.total, 
               old = c ("TOTAL.FU", "BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU"),
               new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))
I.total$ORDER <- "ALL"

## Add ranking:
I.obs <- PROOF (data = I.obs, 
                rank = T, CAFS = T)
I.cfb <- PROOF (data = I.cfb, 
                rank = T, CAFS = T)
I.cafs <- PROOF (data = I.cafs, 
                 rank = T, CAFS = T)
I.total <- PROOF (data = I.total, 
                  rank = T, CAFS = T)
I.obs$R.FU <- I.obs$R
I.obs$RS.FU <- I.obs$RS

I.cfb$R.CFB <- I.cfb$R
I.cfb$RS.CFB <- I.cfb$RS
I.cfb$CFB <- I.cfb$TOTAL

I.cafs$R.CAFS <- I.cafs$R
I.cafs$RS.CAFS <- I.cafs$RS

I.total$R.TOTAL.FU <- I.total$R
I.total$RS.TOTAL.FU <- I.total$RS

B <- merge (B, I.obs[, c ("ID", "R.FU", "RS.FU")], all.x = T)
B <- merge (B, I.cfb[, c ("ID", "R.CFB", "RS.CFB", "CFB")], all.x = T)
B <- merge (B, I.cafs[, c ("ID", "R.CAFS", "RS.CAFS")], all.x = T)
B <- merge (B, I.total[, c ("ID", "R.TOTAL.FU", "RS.TOTAL.FU")], all.x = T)

## Checks:
plot (B$TOTAL, B$TOTAL.FU)
plot (B$R.CFB, B$R.CAFS)
plot (B$R.FU, B$R.TOTAL.FU)
plot (B$R.CAFS, B$TOTAL.FU - B$TOTAL)
plot (B$STIME, B$R.CAFS, col = B$STATUS + 1)
plot (B$TOTAL.FU - B$TOTAL, B$R.CAFS)


mean (B$R.CAFS[B$ONSET == "O"]) - mean (B$R.CAFS[B$ONSET == "B"])
mean (B$R.CAFS[B$STATUS == "1" & B$ONSET == "O"]) - mean (B$R.CAFS[B$STATUS == "1" & B$ONSET == "B"])
mean (B$R.CAFS[B$STATUS == "0" & B$ONSET == "O"]) - mean (B$R.CAFS[B$STATUS == "0" & B$ONSET == "B"])

plot (survfit (Surv (STIME, STATUS) ~ DOMAIN, data = B), mark = "|")
plot (survfit (Surv (STIME, STATUS) ~ ONSET, data = B[B$STATUS == 1, ]))
survdiff (Surv (STIME, STATUS) ~ DOMAIN, data = B)

(((mean (B$R.CAFS[B$STATUS == "1" & B$ONSET == "O"])/101) +
    (mean (B$R.CAFS[B$STATUS == "0" & B$ONSET == "O"])/329))/2)*430


cor.test (B$R.CFB, B$R.CAFS)
cor.test (B$R.FU, B$TOTAL.FU)





## Data used in figure 4
# CFB
adj0 <- 0.75 * (diff (range (I.cafs$TOTAL, na.rm = T))/max (I.cafs$STIME))
x0 <- ifelse (I.cafs$STATUS == 1, 
              (- 2 * adj0) + min (I.cafs$TOTAL, na.rm = T) + adj0 * (I.cafs$STIME - max (I.cafs$STIME)),
              (2 * adj0) + I.cafs$TOTAL)

y0 <- ifelse (I.cfb$STATUS == 1, 
              (- 2 * adj0) + min (I.cfb$TOTAL, na.rm = T) + adj0 * (I.cfb$STIME - max (I.cfb$STIME)),
              (2 * adj0) + I.cfb$TOTAL)

# Total score
adj <- 0.75 * (diff (range (I.total$TOTAL, na.rm = T))/max (I.total$STIME))
x <- ifelse (I.total$STATUS == 1, 
             (- 2 * adj) + min (I.total$TOTAL, na.rm = T) + adj * (I.total$STIME - max (I.total$STIME)),
             (2 * adj) + I.total$TOTAL)

y <- ifelse (I.obs$STATUS == 1, 
             (- 2 * adj) + min (I.obs$TOTAL, na.rm = T) + adj * (I.obs$STIME - max (I.obs$STIME)),
             (2 * adj) + I.obs$TOTAL)



#### 5. Ranking ####
library (Rfit)

fit <- rfit (R.FU ~ R + l.DISDUR + ONSET + AGE + DX + SEX + RILUSE, data = B)
fit1 <- rfit (R.FU ~ R + l.DISDUR + ONSET + AGE + SEX + RILUSE, data = B)

confint.rfit <- function (fit, alpha = 0.05, clean = F){
  
  x <- summary (fit)$coefficients
  df <- length (fit$fitted.values) - nrow (x)
  
  ub <- x[, "Estimate"] + qt (1 - (alpha/2), df) * x[, "Std. Error"]
  lb <- x[, "Estimate"] + qt ((alpha/2), df) * x[, "Std. Error"]
  
  if (clean){
    data.frame (coef = sprintf ("%.2f", x[, "Estimate"]),
                lb = sprintf ("%.2f", lb),
                ub = sprintf ("%.2f", ub), 
                pval = ifelse (x[, "p.value"] < 0.001, "<0.001", sprintf ("%.3f", x[, "p.value"]))) 
  } else {
    data.frame (coef = x[, "Estimate"],
                lb = lb,
                ub = ub, 
                pval = x[, "p.value"]) 
  }
  
}

confint.rfit (fit, clean = T)
drop.test (fit, fit1)


#### Power ####








fit <- rfit (R.TOTAL.FU ~ TOTAL + l.DISDUR + ONSET + AGE + DX + SEX + RILUSE, data = B)
summary(fit)
names (fit)

fit <- lm (R.TOTAL.FU ~ TOTAL + l.DISDUR + ONSET + AGE + DX + SEX + RILUSE, data = B)
confint (fit)
9.0696 - qt (.025, 433)*0.3771

coef (fit)[2] - qt (.025, 424)* summary (fit)$coefficients["TOTAL", c (2)]

summary (fit)$df

coef (fit)[2]



fit$qrx1


plot (fit$fitted.values, B$R.FU)

head (B)
B1 <- B[, c ("TOTAL", "TOTAL.FU")]
B1$CFB <- B1$TOTAL.FU - B1$TOTAL
B1 <- B1[!is.na (B1$CFB), ]

summary (lm (B1$TOTAL.FU ~ B1$TOTAL))
summary (lm (B1$CFB ~ B1$TOTAL))


R <- replicate (1000, {
  
  ## Define dataset
  B.boot <- B1[sample (nrow (B1), size = 100, replace = T), ]
  B.boot$TRT <- rep (c (0,1), each = 50)
  B.boot$CFB <- pmin ((B.boot$TOTAL.FU - B.boot$TOTAL) + (3 * B.boot$TRT), 3)
  B.boot$TOTAL.FU <- B.boot$TOTAL + B.boot$CFB
  
  ## Models:
  w <- wilcox.test (B.boot$TOTAL.FU ~ B.boot$TRT)
  w.cfb <- wilcox.test (B.boot$CFB ~ B.boot$TRT)
  m <- lm (B.boot$TOTAL.FU ~ B.boot$TRT)
  m.cfb <- lm (B.boot$CFB ~ B.boot$TRT)
  m.ancova <- lm (B.boot$TOTAL.FU ~ B.boot$TRT + B.boot$TOTAL)
  m.ancova.rk <- lm (rank (B.boot$TOTAL.FU) ~ B.boot$TRT + rank (B.boot$TOTAL))
  m.cfb.ancova <- lm (rank (B.boot$CFB) ~ B.boot$TRT + rank (B.boot$TOTAL))
  
  data.frame (w = w$p.value,
              w.cfb = w.cfb$p.value,
              m = summary (m)$coefficients["B.boot$TRT", "Pr(>|t|)"],
              m.cfb = summary (m.cfb)$coefficients["B.boot$TRT", "Pr(>|t|)"],
              m.ancova = summary (m.ancova)$coefficients["B.boot$TRT", "Pr(>|t|)"],
              m.ancova.rk = summary (m.ancova.rk)$coefficients["B.boot$TRT", "Pr(>|t|)"],
              m.cfb.ancova = summary (m.cfb.ancova)$coefficients["B.boot$TRT", "Pr(>|t|)"],
              coef = coef (m.cfb)[2])
  
}, simplify = F)

R <- do.call ("rbind", R)
colSums (R < 0.05)/1000
colMeans (R)


## Baseline ranking
B$R.TOTAL <- rank (B$TOTAL) 

cor.test (B$R, B$R.FU, method = "kendall")
cor.test (B$R, B$R.CFB, method = "kendall")

cor.test (B$R.TOTAL, B$R.TOTAL.FU, method = "kendall")
cor.test (B$R, B$R.CAFS, method = "kendall")

## Figure alluvial
table (B$QTL.TOTAL)
with (B[B$STATUS == 0, ], tapply (CFB, QTL.TOTAL, median, na.rm = T))
boxplot (B$CFB ~ B$FT9)

brks.fu <- c (1.0, quantile (sum (B$STATUS):nrow (B), c (0,1/3,2/3,1)))

prop.table (table (B$QTL, factor (cut (B[, "R.CFB"], breaks = brks.fu, include.lowest = T),
                                  labels = c ("Died", "Worst", "Average",  "Best"))), 1)

prop.table (table (B$QTL.TOTAL, factor (cut (B[, "R.CAFS"], breaks = brks.fu, include.lowest = T),
                                        labels = c ("Died", "Worst", "Average",  "Best"))), 1)




library (Rfit)
fit <- rfit (R ~ ONSET, data = B)
summary(fit)

fit <- lm (R ~ ONSET, data = B)
summary(fit)

wilcox.test (R ~ ONSET, data = B)




d <- B[, c ("TOTAL", "CFB", "STIME", "STATUS")]
d$TOTAL.FU <- d$TOTAL + d$CFB
head (d)
d <- d[!is.na (d$TOTAL.FU), ]

## Data
nsim <- 1000
n <- 50
R <- replicate (nsim,{
  
  attempt <- 0
  continue <- T
  
  while (continue){
    d.boot <- d[sample (100,n,replace = T), ]
    d.boot$TRT <- sample (rep (c (0,1), each = n/2))
    d.boot$TOTAL.FU <- ifelse (d.boot$TRT == 1, d.boot$TOTAL.FU+2, d.boot$TOTAL.FU)
    d.boot$CFB <- d.boot$TOTAL.FU - d.boot$TOTAL
    
    pval.cafs <- wilcox.test (CFB ~ TRT, exact = F, data = d.boot)$p.value
    pval.obs <- wilcox.test (TOTAL.FU ~ TRT, exact = F, data = d.boot)$p.value
    
    m <- lm (TOTAL.FU ~ TRT + TOTAL, data = d.boot)
    pval.lm <- summary (m)$coefficients["TRT", "Pr(>|t|)"]
    
    rk <- rfit (TOTAL.FU ~ TRT + TOTAL, data = d.boot)
    pval.rk <- summary (rk)$coefficients["TRT", "p.value"]
    
    
    f0 <- try (orm (TOTAL.FU ~ TOTAL, data = d.boot,  maxit=50L, family = "logistic"), silent = T)
    f <- try (orm (TOTAL.FU ~ TOTAL + TRT, data = d.boot,  maxit=50L, family = "logistic"), silent = T)
    
    attempt <- attempt + 1
    continue <- if (class (f0)[1] == "try-error" | class (f)[1] == "try-error"){T}else{F}
    
  }
  
  pval.ord <- as.numeric (pchisq(-2*(logLik (f0) - logLik (f)), df = 1, lower.tail = F))
  c (pval.cafs,  pval.obs, pval.lm, pval.rk, pval.ord, attempt)
  
})
rowSums (R < 0.05)/nsim
row



library(Rfit)
fit <- rfit (R.FU ~ R, data = B)
summary(fit)


nsim <- 1000
n <- 100
R <- replicate (nsim,{
  
  attempt <- 0
  continue <- T
  
  while (continue){
    
    d.boot <- d[sample (nrow (d),n,replace = T), ]
    d.boot$TRT <- sample (rep (c (0,1), each = n/2))
    d.boot$TOTAL.FU <- ifelse (d.boot$TRT == 1, d.boot$TOTAL.FU+4, d.boot$TOTAL.FU)
    d.boot$CFB <- d.boot$TOTAL.FU - d.boot$TOTAL
    
    d.boot$cafs <- rank (ifelse (d.boot$STATUS == 1, d.boot$STIME - (48 + max (d.boot$STIME)), d.boot$CFB))
    #plot (cafs, d.boot$STIME)
    #plot (cafs, d.boot$CFB)
    
    d.boot$obs <- rank (ifelse (d.boot$STATUS == 1, d.boot$STIME - (48 + max (d.boot$STIME)), d.boot$TOTAL.FU))
    
    
    boxplot (d.boot$CFB ~ d.boot$TRT)
    boxplot (d.boot$TOTAL.FU ~ d.boot$TRT)
    
    
    pval.cafs <- wilcox.test (cafs ~ TRT, exact = F, data = d.boot)$p.value
    pval.obs <- wilcox.test (obs ~ TRT, exact = F, data = d.boot)$p.value
    
    m <- lm (obs ~ TRT + TOTAL, data = d.boot)
    pval.lm <- summary (m)$coefficients["TRT", "Pr(>|t|)"]
    
    rk <- rfit (obs ~ TRT + TOTAL, data = d.boot)
    pval.rk <- summary (rk)$coefficients["TRT", "p.value"]
    
    
    f0 <- try (orm (obs ~ TOTAL, data = d.boot,  maxit=50L, family = "logistic"), silent = T)
    f <- try (orm (obs ~ TOTAL + TRT, data = d.boot,  maxit=50L, family = "logistic"), silent = T)
    
    attempt <- attempt + 1
    continue <- if (class (f0)[1] == "try-error" | class (f)[1] == "try-error"){T}else{F}
    
  }
  
  pval.ord <- as.numeric (pchisq(-2*(logLik (f0) - logLik (f)), df = 1, lower.tail = F))
  c (pval.cafs,  pval.obs, pval.lm, pval.rk, pval.ord, attempt)
  
})
rowSums (R < 0.05)/nsim



R[6,]

plot (cafs, d$CFB)
plot (obs, d$CFB)

require (rms)

f <- orm (d.boot$TOTAL.FU ~ d.boot$TRT)
summary (f)
f


cor.test (d$TOTAL, d$CFB, method = "kendall")








hist (B$RS.FU)

head (B)

par (mar = c (3,3,2,2), mgp = c (1.95, 0.75, 0), mfrow = c (1,2))

plot (rep (1, nrow (B)), B$R, xlim = c (0.75,2.25), 
      col = ifelse (B$STATUS == 1, 2, 1),
      ylab = "PROOF-Based Rank", xlab = "", xaxt = "n", main = "A. Current value")
points (rep (2, nrow (B)), B$R.FU, col = ifelse (B$STATUS == 1, 2, 1))
segments (x0 = 1, x1 = 2, y0 = B$R, y1 = B$R.FU, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .25))
axis (1, at = c (1,2), labels = c ("Baseline", "Month 12"))


plot (rep (1, nrow (B)), B$R, xlim = c (0.75,2.25), 
      col = ifelse (B$STATUS == 1, 2, 1),
      ylab = "PROOF-Based Rank", xlab = "", xaxt = "n", main = "B. Change from baseline")
points (rep (2, nrow (B)), B$R.CFB, col = ifelse (B$STATUS == 1, 2, 1))
segments (x0 = 1, x1 = 2, y0 = B$R, y1 = B$R.CFB, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .25))
axis (1, at = c (1,2), labels = c ("Baseline", "Month 12"))


sum (B$R.FU[B$R < median (B$R)] < B$R[B$R < median (B$R)])
sum (B$R.FU[B$R < median (B$R)] == B$R[B$R < median (B$R)])
sum (B$R.FU[B$R < median (B$R)] > B$R[B$R < median (B$R)])

sum (B$R.FU[B$R >= median (B$R)] < B$R[B$R >= median (B$R)])
sum (B$R.FU[B$R >= median (B$R)] == B$R[B$R >= median (B$R)])
sum (B$R.FU[B$R >= median (B$R)] > B$R[B$R >= median (B$R)])



sum (B$R.CFB[B$R < median (B$R)] < B$R[B$R < median (B$R)])
sum (B$R.CFB[B$R < median (B$R)] == B$R[B$R < median (B$R)])
sum (B$R.CFB[B$R < median (B$R)] > B$R[B$R < median (B$R)])

sum (B$R.CFB[B$R >= median (B$R)] < B$R[B$R >= median (B$R)])
sum (B$R.CFB[B$R >= median (B$R)] == B$R[B$R >= median (B$R)])
sum (B$R.CFB[B$R >= median (B$R)] > B$R[B$R >= median (B$R)])

B$QTL <- cut (B$R, quantile (1:nrow (B)), include.lowest = T)
B$QTL.TOTAL <- cut (rank (B$TOTAL), quantile (1:nrow (B)), include.lowest = T)

prop.table (table (B$QTL, B$R.FU > B$R), 1)
prop.table (table (B$QTL, B$R.CFB > B$R), 1)

table (B$QTL, B$QTL.TOTAL)

plot (survfit (Surv (STIME, STATUS) ~ QTL, data = B),
      mark = "|", col = cols (4),
      ylab = "Probability of survival", xlab = "Time since first survey (months)",
      main = "PROOF")
legend ("bottomleft", bty = "n", legend = c ("1-108 (N = 109)", "109-216 (N = 108)",
                                             "217-325 (N = 108)", "325-433 (N = 108)"),
        pch = 15, col = cols (4))
plot (survfit (Surv (STIME, STATUS) ~ QTL.TOTAL, data = B), 
      mark = "|", col = cols (4),
      ylab = "Probability of survival", xlab = "Time since first survey (months)",
      main = "ALSFRS-R total score")
legend ("bottomleft", bty = "n", legend = c ("1-108 (N = 116)", "109-216 (N = 109)",
                                             "217-325 (N = 104)", "325-433 (N = 104)"),
        pch = 15, col = cols (5))


table (B$QTL, B$QTL.TOTAL)

prop.table (table (B$QTL, B$R.FU > B$R), 1)
prop.table (table (B$QTL, B$R.CFB > B$R), 1)

prop.table (table (B$QTL.TOTAL, B$R.TOTAL > rank (B$TOTAL)), 1)
prop.table (table (B$QTL.TOTAL, B$R.CAFS > rank (B$TOTAL)), 1)

tapply (rank (B$TOTAL), B$STATUS, mean)
tapply (B$R, B$STATUS, mean)

plot (B$R, B$R.FU)


prop.table (table (substr (B$ORDER, 1, 1), B$QTL),2)

cor.test (B$R, B$R.CFB, method = "kendall")


plot (rank (B$TOTAL), B$R.TOTAL, col = adjustcolor (B$STATUS + 1, .25), pch = 16, xlab = "Baseline rank", ylab = "Month 12 rank")


summary (lm (R.TOTAL ~ rank (TOTAL) + ONSET, data = B))
summary (lm (R.TOTAL ~ ONSET, data = B))
summary (lm (R.CAFS ~ ONSET, data = B))
summary (lm (R.TOTAL - rank (TOTAL) ~ ONSET + rank (TOTAL), data = B))


m <- lm (R.FU ~ R, data = B)
summary (m)

library(Rfit)
fit <- rfit (R.FU ~ R, data = B)
summary(fit)

plot (B$R, B$R.FU)
abline (m)
abline (fit)
summary (m)

plot (fitted (fit), rstudent (fit))
points (fitted (m), residuals (m), col = 2)
qqnorm (rstudent (fit))
# https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Kloke+McKean.pdf
m <- lm (R.FU ~ R + ONSET, data = B)
plot (fitted (m), residuals (m), col = B$STATUS+1)

par (mar = c (3,3,2,2), mgp = c (1.95, 0.75, 0), mfrow = c (1,2))
plot (rep (1, nrow (B)), rank (B$TOTAL), xlim = c (0.75,2.25), 
      col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .15),
      pch = 16,
      ylab = "CAFS score", xlab = "", xaxt = "n",
      main = "A. Change from baseline")
points (rep (2, nrow (B)), B$R.CAFS, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .15), pch = 16)
segments (x0 = 1, x1 = 2, y0 = B$R, y1 = B$R.CAFS, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .25))
axis (1, at = c (1,2), labels = c ("Baseline", "Month 12"))

plot (rep (1, nrow (B)), rank (B$TOTAL), xlim = c (0.75,2.25), 
      col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .15),
      pch = 16,
      ylab = "CAFS score", xlab = "", xaxt = "n", main = "B. Observed value")
points (rep (2, nrow (B)), B$R.TOTAL, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .15), pch = 16)
segments (x0 = 1, x1 = 2, y0 = rank (B$TOTAL), y1 = B$R.TOTAL, col = adjustcolor (ifelse (B$STATUS == 1, 2, 1), .25))
axis (1, at = c (1,2), labels = c ("Baseline", "Month 12"))





plot (B$TOTAL, B$R.CAFS - rank (B$TOTAL), col = B$STATUS + 1, ylim = c (-433, 433),
      ylab = "Change in rank", xlab = "Baseline ALSFRS-R")
plot (B$TOTAL, B$R.TOTAL - rank (B$TOTAL), col = B$STATUS + 1, ylim = c (-433, 433),
      ylab = "Change in rank", xlab = "Baseline ALSFRS-R")

plot ( B$TOTAL, B$TOTAL.FU - B$TOTAL, col = B$STATUS + 1)

wilcox.test (B$R.TOTAL ~ B$ONSET)
wilcox.test (B$R.CAFS ~ B$ONSET)
wilcox.test (B$R.CFB ~ B$ONSET)
wilcox.test (B$R.FU ~ B$ONSET)

m <- lm (B$RS.FU ~ B$ONSET + B$RS)
m <- lm (B$R.CAFS ~ B$ONSET + rank (B$TOTAL))
summary (m)

plot (fitted (m), residuals (m))


cor.test (B$TOTAL, B$R.CAFS, method = "spearman")


#### Simulation ####
boxplot (B$R-B$R.FU ~ B$DX)

R <- mclapply (1:1000,function (ii) {
  
  n <- sample (nrow (I), 100)
  n1 <- n[1:50]
  n2 <- n[51:100]
  
  ## TRT:
  I.sim1 <- I[n1, ]
  I.sim1$TRT <- 1
  I.sim1$BULBAR.FU <- I.sim1$BULBAR.FU + 1
  I.sim1$FINE.FU <- I.sim1$FINE.FU + 1
  I.sim1$GROSS.FU <- I.sim1$GROSS.FU + 1
  I.sim1$RESP.FU <- I.sim1$RESP.FU + 1
  I.sim1$TOTAL.FU <- rowSums (I.sim1[, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")])
  
  ## Control
  I.sim2 <- I[n2, ]
  I.sim2$TRT <- 0
  I.sim <- rbind (I.sim1, I.sim2)
  
  ## Calculate stats:
  I.sim$CFB <- I.sim$TOTAL.FU - I.sim$TOTAL
  
  I.sim$TOTAL.CFB <- I.sim$TOTAL.FU - I.sim$TOTAL
  I.sim$BULBAR.CFB <- I.sim$BULBAR.FU - I.sim$BULBAR
  I.sim$FINE.CFB <- I.sim$FINE.FU - I.sim$FINE
  I.sim$GROSS.CFB <- I.sim$GROSS.FU - I.sim$GROSS
  I.sim$RESP.CFB <- I.sim$RESP.FU - I.sim$RESP
  
  ## Define datasets:
  I.sim.cfb <- I.sim[, c ("ID", "TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB", "ORDER", "STIME", "STATUS", "TRT")]
  I.sim.cfb <- rn (I.sim.cfb, 
                   old = c ("TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB"),
                   new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))
  
  ## total
  I.sim.cfb.t <- I.sim[, c ("ID", "TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB", "ORDER", "STIME", "STATUS", "TRT")]
  I.sim.cfb.t <- rn (I.sim.cfb.t, 
                     old = c ("TOTAL.CFB", "BULBAR.CFB", "FINE.CFB", "GROSS.CFB", "RESP.CFB"),
                     new = c ("TOTAL", "BULBAR", "FINE", "GROSS", "RESP"))
  I.sim.cfb.t$ORDER <- "ALL"
  
  
  ## Tests:
  singleTest <- PROOF (data = I.sim.cfb, 
                       grp.var = "TRT",
                       ctrl.cat = 0, 
                       Single = T,
                       CAFS = T)
  
  singleTest1 <- PROOF (data = I.sim.cfb, 
                        grp.var = "TRT",
                        ctrl.cat = 0, 
                        Single = T,
                        CAFS = T, delta = 1)
  
  classicCAFS <- PROOF (data = I.sim.cfb.t, 
                        grp.var = "TRT", 
                        ctrl.cat = 0, 
                        CAFS = T)
  
  
  rbind ("singleTest" = singleTest,
         "singleTest1" = singleTest1,
         "classicCAFS" = classicCAFS)
  
  
}, mc.cores = 4)

rowSums (sapply (R, function (d){d$pval}) < 0.05)/1000








par (mar = c (3,3,2,2), mgp = c (1.95, 0.75, 0))
plot (NULL, xlim = c (0, 12.5), ylim = c (0,48), ylab = "ALSFRS-R", xlab = "Time (months)")
segments (x0 = 0, y0 = 40,
          x1 = 12, y1 = 20)
polygon (x =  c (0,0,12,12),
         y = c (0,40,20,0),
         col = adjustcolor (1, .1), border = NA)





head (I1)


head (I)


plot (I.cfb$R, I.cfb$STIME,
      ylab = "Survival time (months)",
      xlab = "PROOF-rank",
      col = adjustcolor (B$STATUS + 1, .25),
      pch = 16)
abline (0, 1, lty = 3)


hist (I.cfb.t$TOTAL, breaks = 100)
length (unique (I.cfb.t$TOTAL))
length (unique (I.cfb.t$R[B$STATUS == 0]))

plot (as.Date (B$DATE.FU))
B[B$FU == "NO RESPONSE", ]$ID
plot (predict (m_bulbar), B[!is.na (B$BULBAR.FU), ]$BULBAR.FU)

head (B)


plot (B$RESP, B$RESP.FU)

table (B$STATUS)


table (D$APROACH)
(nrow (D) - sum (D$APROACH == "NO RESPONSE")) / nrow (D)
B <- D[D$APROACH == "COMPLETED", ]
table (B$APROACH.RT)
prop.table (table (B$DX))
B$RT <- is.na (B$I3.RT)

B$DX <- factor (B$DX, levels = c ("ALS", "PMA", "PLS"))
tableone::CreateTableOne (vars = c ("AGE", "SEX", "ONSET",
                                    "DISDUR", "DXDELAY",
                                    "TOTAL", "SLOPE",
                                    "BULBAR", "FINE", "GROSS", "RESP"),
                          strata = "RT", data = B, test = T, addOverall = T)

tableone::CreateTableOne (vars = c ("AGE", "SEX", "ONSET",
                                    "DISDUR", "DXDELAY",
                                    "TOTAL", "SLOPE",
                                    "BULBAR", "FINE", "GROSS", "RESP"),
                          strata = "DX", data = B, test = F, addOverall = T)

par (mfrow = c (1,2))
hist (D$TOT.TIME, breaks = 100, xlim = c (0, 80), xlab = "Time to complete survey (minutes)",
      main = "Survey 1", ylim = c (0,40))
text (40, 20, paste0 ("Median = ", median (B$TOT.TIME), " minutes"), font = 2)

hist (D$TOT.TIME.RT, breaks = 100, xlim = c (0, 45), xlab = "Time to complete survey (minutes)",
      main = "Survey 2 (retest)", ylim = c (0,40))
text (22.5, 20, paste0 ("Median = ", median (B$TOT.TIME.RT, na.rm = T), " minutes"), font = 2)

par (mfrow = c (1,2))
boxplot (B$PH ~ B$M, ylim = c (10, 70), main = "Physical health",
         ylab = "US-standardized score", xlab = "MiToS clinical stage")
abline (h = 50, lty = 3)
boxplot (B$MH ~ B$M, ylim = c (10, 70), main = "Mental health",
         ylab = "US-standardized score", xlab = "MiToS clinical stage")
abline (h = 50, lty = 3)

## So if the lower correlation because QoL is mostly related to Fine & Gross and those are ranked of lower importance?
cor.test (B$PH, B$BULBAR, method = "spearman", exact = F)
cor.test (B$PH, B$FINE, method = "spearman", exact = F)
cor.test (B$PH, B$GROSS, method = "spearman", exact = F)
cor.test (B$PH, B$RESP, method = "spearman", exact = F)

cor.test (B$MH, B$BULBAR)
cor.test (B$MH, B$FINE)
cor.test (B$MH, B$GROSS)
cor.test (B$MH, B$RESP)


plot (B$TOTAL, B$R, ylab = "PROOF rank", xlab = "ALSFRS-R total score")

par (mfrow = c (1,2))
plot (B$TOTAL, B$PH, ylim = c (10, 70), main = "ALSFRS-R",
      ylab = "US-standardized score", xlab = "ALSFRS-R total score")
abline (h = 50, lty = 3)
plot (B$R, B$PH, ylim = c (10, 70), main = "PROOF",
      ylab = "US-standardized score", xlab = "PROOF rank")
abline (h = 50, lty = 3)

cor.test (B$PH, B$TOTAL, method = "spearman", exact = F)
cor.test (B$PH, B$R, method = "spearman", exact = F)


boxplot (B$TOTAL ~ B$G01 + B$DX)

B$ORDER
Ord <- sapply (B$ORDER, function (ii){
  
  if (ii == "ALL"){"A"} else {substr (ii, 1, 1)}
  
})

mat <-  (t (sapply (B$ORDER, function (ii){
  if (ii == "ALL") {c (2.5, 2.5, 2.5, 2.5)} else {(1:4)[order (str_split (ii, "")[[1]])]}
})))
colMeans (mat)

wilcox.test (mat[,1], mu = 2.5)
table (mat[,1])
table (mat[,2])
table (mat[,3])
table (mat[,4])

prop.table (table (Ord))


P.rank <- function (data, id.var){
  
  ## Calculate PROOF
  ids <- data[, id.var]
  n <- length (ids)
  
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
    
    # Define comparison sets: which sets are common?
    C <- Com (i = i, j = j)
    
    # For each common set, who is the winner?
    Eval.NoMemory (C = C, i = i, j = j) 
    
  }, mc.cores = 4)
  comparisons <- unlist (comparisons, use.names = F)
  
  # Redefine Z for numeric input
  Z <- matrix (NA, ncol = n, nrow = n)
  diag (Z) <- 0.5 ## Each patient is equal to itself
  colnames (Z) <- rownames (Z) <- ids
  
  # Add the comparisons to lower half of matrix
  Z[lower.tri (Z)] <- as.numeric (comparisons)
  
  # Inverse output for upper part:
  Z1 <- t (Z)
  Z1[lower.tri (Z)] <- ifelse (Z[lower.tri (Z)] == 1, 0,
                               ifelse (Z[lower.tri (Z)] == 0, 1, 0.5))
  Z <- t (Z1)
  
  ### End calculation of Z matrix ###
  ## Reference material: https://en.wikipedia.org/wiki/Mann–Whitney_U_test
  ## Estimation follows indirect method
  
  ## Step 1: Rank patients
  return (rank (rowSums (Z)))
  
}

head (B1)
B1 <- B[, c ("ID", "TOTAL.RT", "BULBAR.RT", "FINE.RT", "GROSS.RT", "RESP.RT", "ORDER.RT")]
B1 <- B[, c ("ID", "ALSAQ", "A03", "A02", "A01", "A0X", "ORDER")]
names (B1) <- c ("ID", "TOTAL", "BULBAR", "FINE", "GROSS", "RESP", "ORDER")

R <- P.rank (data = B, id.var = "ID")
R.RT <- P.rank (data = B1, id.var = "ID")

B$R <- R
B$R.RT <- R.RT

par (mfrow = c (1,2))
cor.test (rank (B$R), rank (B$R.RT))
plot (B$ALSAQ, R.RT)

cor.test (B$ALSAQ, B$TOTAL)
cor.test (B$R.RT, B$TOTAL)

B1 <- B[B$DX == "ALS", ]


cor.test (B$ALSAQ, B$PH)

plot (rank (B1$R), B1$ALSAQ.X)
cor.test (B1$TOTAL, B1$ALSAQ.X, method = "spearman")

plot (rowMeans (cbind (R, R.RT)), R-R.RT)
abline (h = 0)
sd (R-R.RT)

boxplot (B$PH ~ B$HELP)

which (abs (R-R.RT) > 175)

B[B$ID == "ALS00569", c ("TOTAL", "TOTAL.RT", "RESP", "RESP.RT", "ORDER", "ORDER.RT")]
B[B$ID == "ALS01729", c ("TOTAL", "TOTAL.RT", "FINE", "RESP.RT", "ORDER", "ORDER.RT")]
B[B$ID == "ALS36104", c ("TOTAL", "TOTAL.RT", "RESP", "RESP.RT", "ORDER", "ORDER.RT")]
B[B$ID == "ALS36622", c ("TOTAL", "TOTAL.RT", "RESP", "RESP.RT", "ORDER", "ORDER.RT")]



cor.test (B[B$ORDER == "ALL", ]$R, B[B$ORDER == "ALL", ]$PH)
cor.test (B[B$ORDER == "ALL", ]$TOTAL, B[B$ORDER == "ALL", ]$PH)



cor.test (B$BULBAR, B$BULBAR.RT)
cor.test (B$FINE, B$FINE.RT)
cor.test (B$GROSS, B$GROSS.RT)
cor.test (B$RESP, B$RESP.RT)

prop.table (table (B$ORDER == "ALL"))
prop.table (table (B$ORDER.RT == "ALL"))
table (B$ORDER == "ALL", 
       B$ORDER.RT == "ALL")



dev.off ()
cor.test (B$TOTAL, B$TOTAL.RT)
cor.test (R, R.RT)

plot (B$R, R)

cor.test (B$R, B$PH)
cor.test (R, B$PH)
cor.test (B$TOTAL, B$MH, method = "spearman", exact = F)
cor.test (B$TOTAL, B$ALSAQ.X, method = "spearman")
cor.test (B$PROOF, B$ALSAQ.X, method = "spearman")

plot (B$R, B$PH)
cor.test (B$R, B$MH/B$EQ5D)
cor.test (B$TOTAL, B$MH/B$EQ5D)

boxplot (rank (B$ALSAQ) ~ B$DOMAIN)

head (B)

cor.test (rank (B$TOTAL), B$EQ5D)


cor.test (B$I12, B$I12.RT)
mean (B$TOTAL)
mean (B$TOTAL.RT, na.rm = T)
# This the source code

