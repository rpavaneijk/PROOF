#### Preference IIa ####
# MS Title: Combining patient preferences, function and survival in clinical trials for ALS
# written by van Eijk RP (2023)


#### 0. Source ####
source ("https://raw.githubusercontent.com/rpavaneijk/Basics/master/Source_Basics.R")
source ("Source_PROOF.R")


#### 1. Data ####
B <- read.csv("Preference_IIa_AnalysisSet.csv")
B$MOST <- substr (B$ORDER, 1,1)


#### 2. Survival ####

#. Log-rank reported in paper:
summary (survfit (Surv (STIME, STATUS) ~ 1, data = B), time = 12)
survdiff (Surv (STIME, STATUS) ~ DOMAIN, data = B)
survdiff (Surv (STIME, STATUS) ~ MOST, data = B)

#. Adjusted p-val:
drop1 (coxph (Surv (STIME, STATUS) ~ DOMAIN + TOTAL, data = B), test = "Chisq")
drop1 (coxph (Surv (STIME, STATUS) ~ MOST + TOTAL, data = B), test = "Chisq")

## Figure 3 manuscript ##
km_domain <- survfit (Surv (STIME, STATUS) ~ DOMAIN, se = F, data = B)
km_most <- survfit (Surv (STIME, STATUS) ~ MOST, data = B)

#. Table at risk table
time <- c (0,3,6,9,12,15)
evts_domain <- At.Risk (data = B, grp.var = "DOMAIN", time = time, clean = T)
evts_most <- At.Risk (data = B, grp.var = "MOST", time = time, clean = T)

#. Code figure in Figures/Fig3.R


#### 3. PROOF ####
## Below not in manuscript ##

## Expanded function PROOF ##
#. allows comparisons on either 1-4 levels of preference, other ordering variables, total score only:
R_lvl1 <- PROOF (B, n.pref = 1)
R_lvl4 <- PROOF (B, n.pref = 4)
R_Bothersome <- PROOF (B, n.pref = 1, order = "DOMAIN")
R_Total <- PROOF (B, total.only = T)

#. Verification:
plot (rank (B$TOTAL), R_Total$R, xlab = "Ranked total score", ylab = "PROOF function output")
plot (R_lvl1$R, R_lvl4$R, xlab = "Level 1 comparison", ylab = "Level 4 comparison")

#. Correlations between ranks
plot (data.frame (PROOF_lvl1 = R_lvl1$R,
                  PROOF_lvl4 = R_lvl4$R,
                  PROOF_Bothersome = R_Bothersome$R,
                  Total_Score = R_Total$R))

#. Number of unique comparisons:
(nrow (B)^2 - nrow (B))/2
nrow (R_lvl4$raw)
n <- seq (50, 500, by = 10)
plot (n, ((n^2 - n)/2), type = "l", xlab = "Sample size", ylab = "Number of unique comparisons")

#. Number of comparisons based on FRS on first comparison [both pairs had 'ALL']
#. = probability of ORDER equal to "ALL" for a pair of patients
table (R_lvl4$raw$Var == "TOTAL" & R_lvl4$raw$Iter == 1)/((nrow (B)^2 - nrow (B))/2)
(table (B$ORDER == "ALL")[2] / nrow (B)) * ((table (B$ORDER == "ALL")[2] - 1) / (nrow (B)-1))

#. Benefit of 1 or 4 level comparisons, reducing number of ties:
table (R_lvl4$raw$Var == "TOTAL" & R_lvl4$raw$Iter == 2) # number of domain comparisons that resulted in tie
table (R_lvl1$raw$Var == "TOTAL" & R_lvl1$raw$Iter == 2) # additional comparisons helped to break tie:
# Reducing number ties on domains from 16916 to 9789 (-42%)

#. Tough most ties can still be broken by total score:
table (R_lvl4$raw$Z)
table (R_lvl1$raw$Z)


#### 4. Combined ####
CAFS <- PROOF (B, n.pref = 1,
               CAFS = T,
               total.score = "TOTAL.CFB",
               total.only = T)
RKS.BASELINE <- PROOF (B)

#. Verification CAFS:
plot (B$STIME[B$STATUS == 1], CAFS$R[B$STATUS == 1], xlab = "Survival time", ylab = "Patient rank")
plot (B.imp$TOTAL.CFB[B$STATUS == 0], CAFS$R[B$STATUS == 0], xlab = "Change from baseline", ylab = "Patient rank")

#. Follow-up PROOF ranks

## Using change from baseline:
RKS.CFB <- PROOF (B,
                  CAFS = T,
                  total.score = "TOTAL.CFB",
                  list.preference = list (
                    B = "BULBAR.CFB",
                    F = "FINE.CFB",
                    G = "GROSS.CFB",
                    R = "RESP.CFB"
                  ))

## Using observed score at month 12
RKS.FU <- PROOF (B,
                 CAFS = T,
                 total.score = "TOTAL.FU",
                 list.preference = list (
                   B = "BULBAR.FU",
                   F = "FINE.FU",
                   G = "GROSS.FU",
                   R = "RESP.FU"
                 ))

## Figure 4 manuscript ##

#. Define quantiles:
brks <- quantile (1:nrow (B))
brks.fu <- c (1, sum (B$STATUS) + quantile (1:(nrow (B) - sum (B$STATUS))))
base <- factor (cut (RKS.BASELINE$R, breaks = brks, include.lowest = T), 
                labels = c ("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))

#. Calculate changes over time:
t_12.cfb <- factor (cut (RKS.CFB$R, breaks = brks.fu, include.lowest = T),
                labels = c ("Died", "Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))
tab.cfb <- table (base, t_12.cfb)

t_12.fu <- factor (cut (RKS.FU$R, breaks = brks.fu, include.lowest = T),
                labels = c ("Died", "Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))
tab.fu <- table (base, t_12.fu)

plt <- expand.grid (T1 = levels (base), 
                    T2 = levels (t_12.cfb))

#. Code for figure see Figures/Fig4.R

#. Death rates reported in text:
chisq.test (table (base, B$STATUS))
round (100*prop.table (table (base, B$STATUS), 1)[,2], 1)

#. Correlation between baseline & follow-up:
cor.test (RKS.BASELINE$R, RKS.FU$R)
cor.test (RKS.BASELINE$R, RKS.CFB$R)


#### 5. RCT Example ####

## Simulate a clinical trial dataset by means of resampling ##

set.seed (244)
B.sim_trt <- B[sample (nrow (B), size = 50, replace = T), ]
B.sim_con <- B[sample (nrow (B), size = 50, replace = T), ]
B.sim_trt$TRT <- 1; B.sim_con$TRT <- 0

#. Add a treatment benefit
B.sim_trt$BULBAR.FU <- B.sim_trt$BULBAR.FU + 2
B.sim_trt$FINE.FU  <- B.sim_trt$FINE.FU
B.sim_trt$GROSS.FU <- B.sim_trt$GROSS.FU 
B.sim_trt$RESP.FU <- B.sim_trt$RESP.FU + 2 

#. Combine arms
B.sim <- rbind (B.sim_trt,
                B.sim_con)
B.sim$ID <- 1:nrow (B.sim)
B.sim$TOTAL.FU <- rowSums (B.sim[, c ("BULBAR.FU", "FINE.FU", "GROSS.FU", "RESP.FU")])

#. Function to summarize functional benefit 
trt.effect <- function (B.sim){
  
  t.bulb <- t.test (B.sim$BULBAR.FU ~ B.sim$TRT == 0)
  t.fine <- t.test (B.sim$FINE.FU ~ B.sim$TRT == 0)
  t.gross <- t.test (B.sim$GROSS.FU ~ B.sim$TRT == 0)
  t.resp <- t.test (B.sim$RESP.FU ~ B.sim$TRT == 0)
  t.total <- t.test (B.sim$TOTAL.FU ~ B.sim$TRT == 0)
  
  d <- data.frame (Outcome = c ("Total", "Bulbar", "Fine", "Gross", "Resp"),
              Mean_Treatment = c (round (t.total$estimate[1], 1),
                                  round (t.bulb$estimate[1], 1),
                                  round (t.fine$estimate[1], 1),
                                  round (t.gross$estimate[1], 1),
                                  round (t.resp$estimate[1], 1)),
              Mean_Control = c (round (t.total$estimate[2], 1),
                                round (t.bulb$estimate[2], 1),
                                round (t.fine$estimate[2], 1),
                                round (t.gross$estimate[2], 1),
                                round (t.resp$estimate[2], 1)),
              Mean_difference = c (round (diff (-t.total$estimate), 1),
                                   round (diff (-t.bulb$estimate), 1),
                                   round (diff (-t.fine$estimate), 1),
                                   round (diff (-t.gross$estimate), 1),
                                   round (diff (-t.resp$estimate), 1)),
              CI = c (paste (my.round (t.total$conf.int,1), collapse = " - "),
                      paste (my.round (t.bulb$conf.int,1), collapse = " - "),
                      paste (my.round (t.fine$conf.int,1), collapse = " - "),
                      paste (my.round (t.gross$conf.int,1), collapse = " - "),
                      paste (my.round (t.resp$conf.int,1), collapse = " - ")
              ),
              pval = c (                        round (t.total$p.value, 3),
                                                round (t.bulb$p.value, 3),
                                                round (t.fine$p.value, 3),
                                                round (t.gross$p.value, 3),
                                                round (t.resp$p.value, 3)
              ))
  
  d$pval <- ifelse (d$pval < 0.001, "<0.001", ifelse (d$pval > 0.20, my.round (d$pval, 2), my.round (d$pval, 3)))
  d
}

#. Summary results:
plot (survfit (Surv (STIME, STATUS) ~ TRT, data = B.sim), mark = "|",
      col = 1:2)
summary (coxph (Surv (STIME, STATUS) ~ TRT, data = B.sim))
trt.effect (B.sim)

#. Distribution of preferences:
prop.table (table (B.sim$MOST))
table (B.sim$DOMAIN)

#. Estimating combined statistics
CAFS <- PROOF (B.sim, n.pref = 1,
               CAFS = T,
               total.score = "TOTAL.FU",
               total.only = T,
               grp.var = "TRT")

PROOF_imp <- PROOF (B.sim, n.pref = 1,
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

PROOF_bot <- PROOF (B.sim, n.pref = 1,
                    order = "DOMAIN",
                    CAFS = T,
                    total.score = "TOTAL.FU",
                    total.only = F,
                    list.preference = list (
                      B = "BULBAR.FU",
                      F = "FINE.FU",
                      G = "GROSS.FU",
                      R = "RESP.FU"),
                    grp.var = "TRT")

#. Results reported in figure:
CAFS$Results
PROOF_imp$Results
PROOF_bot$Results

#. Code figure 5 see Figures/Fig5.R