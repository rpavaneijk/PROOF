#### 0. Functions ####
roundedRect <- function(
    xleft, ybottom, xright, ytop,
    rounding=0.25,
    bothsame=TRUE,
    aspcorrect=bothsame,
    devcorrect=bothsame,
    corfactor=1.3,
    factorpoints=FALSE,
    corners=1:4,
    npoints=200,
    plot=TRUE,
    ...)
{
  # abbreviated inputs and checks:
  XL <- xleft
  XR <- xright
  YB <- ybottom
  YT <- ytop
  RR <- rounding
  if(length(XL)>1) stop(   "xleft must be a single value, not ", length(XL))
  if(length(XR)>1) stop(  "xright must be a single value, not ", length(XR))
  if(length(YB)>1) stop( "ybottom must be a single value, not ", length(YB))
  if(length(YT)>1) stop(    "ytop must be a single value, not ", length(YT))
  if(length(RR)>1) stop("rounding must be a single value, not ", length(RR))
  if(RR>1) warning("rounding recommended to be smaller than 1. It is ", RR)
  if(RR<0) warning("rounding recommended to be larger than 0. It is ", RR)
  if(!is.numeric(corners)) stop("corners must be ingtegers, not ", class(corners))
  if(!all(corners %in% 0:4)) stop("corners must be (some of) the integers 0:4, not ", 
                                  toString(corners))
  
  XD <- XR-XL
  YD <- YT-YB
  if(bothsame) XD <- YD <- min(c(XD,YD))
  xi <- RR*(XD) # x inset of rounded corner
  yi <- RR*(YD)
  
  asp <- diff(par("usr")[3:4])/diff(par("usr")[1:2]) # current aspect ratio y to x
  dev <- dev.size()[1]/dev.size()[2]
  if(aspcorrect) xi <- xi/asp
  if(devcorrect) xi <- xi/dev
  xi <- xi/corfactor
  
  # elliptic corners function:
  elx <- function(from,to) xi*cos(seq(from,to,length.out=npoints/4))
  ely <- function(from,to) yi*sin(seq(from,to,length.out=npoints/4))
  
  # x and y coordinates:
  xc <- c(if(3 %in% corners) XR-xi+elx(0     ,pi/2  ) else XR, # corner 3 TR
          if(2 %in% corners) XL+xi+elx(pi/2  ,pi    ) else XL, # corner 2 TL
          if(1 %in% corners) XL+xi+elx(pi    ,3*pi/2) else XL, # corner 1 BL
          if(4 %in% corners) XR-xi+elx(3*pi/2,2*pi  ) else XR) # corner 4 BR
  yc <- c(if(3 %in% corners) YT-yi+ely(0     ,pi/2  ) else YT, # corner 3 TR
          if(2 %in% corners) YT-yi+ely(pi/2  ,pi    ) else YT, # corner 2 TL
          if(1 %in% corners) YB+yi+ely(pi    ,3*pi/2) else YB, # corner 1 BL
          if(4 %in% corners) YB+yi+ely(3*pi/2,2*pi  ) else YB) # corner 4 BR
  if(plot) polygon(x=xc, y=yc, ...)
  if(factorpoints) points(c(XL+xi, XL+xi, XR-xi, XR-xi), 
                          c(YB+yi, YT-yi, YT-yi, YB+yi), pch=3, col=2, lwd=1)
  invisible(data.frame(x=xc,y=yc))
}

## Prelims:
col.km <- gg.cols (2)
es <- trt.effect (B.sim)
rank.t <- my.round (c (CAFS$Results$R.trt, PROOF_imp$Results$R.trt, PROOF_bot$Results$R.trt), 1)
rank.c <- my.round (c (CAFS$Results$R.ctrl, PROOF_imp$Results$R.ctrl, PROOF_bot$Results$R.ctrl), 1)
rank.diff <- my.round (c (CAFS$Results$R.diff, PROOF_imp$Results$R.diff, PROOF_bot$Results$R.diff), 1)
wp <- my.round (c (CAFS$Results$theta, PROOF_imp$Results$theta, PROOF_bot$Results$theta), 2)
or <- my.round (c (CAFS$Results$OR, PROOF_imp$Results$OR, PROOF_bot$Results$OR), 2)
nnt <- my.round (c (CAFS$Results$NNT, PROOF_imp$Results$NNT, PROOF_bot$Results$NNT), 2)
pval <- my.round (c (CAFS$Results$pval, PROOF_imp$Results$pval, PROOF_bot$Results$pval), 3)


pdf (file = "/Users/rpavaneijk/SurfDrive/Writing/Artikelen/ACTIVE/1. PROOF Sequel/Figures/FIG5.pdf", width = 7, height = 8)

par (mar = c (26,7,2,6), mgp = c (2.25, 0.75, 0))
plot (survfit (Surv (STIME, STATUS) ~ TRT, data = B.sim), xlim = c (0,14),
      ylim = c (0, 1.025), axes = F, 
      ylab = "Overall survival (%)", 
      xlab = "", col = col.km, lwd = 1.25,
      xaxs = "i", yaxs = "i", mark = "|")
title (xlab = "Time since randomization (months)", mgp = c(2.0, 1, 0))

axis (1, seq (-6,36, by = 3)); axis (2, seq (0, 1, by = 0.25), 
                                     labels = c ("0", "25", "50", "75", "100"), las = 1)
legend ("bottomleft", 
        legend = c ("Placebo (n = 50)", "Treated (n = 50)"),
        lty = 1, col = col.km, bty = "n", lwd = 1.5, cex = 0.975)

text (.4, .26, adj = 0, paste0 ("HR 0.52 (0.23 - 1.18), p = 0.119"))

mtext (expression (bold ("A.") ~ "Survival outcome"), adj = -0.35, line = 0.75)


## Panel B ALSFRS-R outcomes
mtext (expression (bold ("B.") ~ "ALSFRS-R outcome"), adj = -0.39, line = -16.25)

roundedRect (xleft = -2.25, ybottom = -1.15, xright = 15.25, ytop = -.4, xpd = T,
             rounding = .1, lwd = 1, col = adjustcolor (1, .05))


text (c (2.75, 5, 7.5, 10.5, 13.5), -.50,
      c ("Mean\ntreated", "Mean\nplacebo", "Mean\ndifference", "95% CI\n", "P-value\n"),
      xpd = T, font = 2, cex = 0.95)

text (c (-1.85, -1.45, -1.45, -1.45, -1.45), c (-.675, -.775, -.875, -.975, -1.075),
      c ("Total score", "Bulbar", "Fine motor", "Gross motor", "Respiratory"),
      adj = 0, xpd = T, cex = .95)

segments (x0 = -1.85, y0 = -.61, x1 = 14.75, y1 = -.61, xpd = T)


es$Mean_Treatment <- sprintf ("%.1f", es$Mean_Treatment )
es$Mean_Control <- sprintf ("%.1f", es$Mean_Control)
es$Mean_difference <- sprintf ("%.1f", es$Mean_difference)

text (t (sapply (1:5, function (i){c (2.75, 5, 7.5, 10.5, 13.5)})),
  sapply (1:5, function (i){c (-.675, -.775, -.875, -.975, -1.075)}),
  do.call ("c", c ((es[,2:6]))), xpd = T, cex = 0.95)

## Panel C Combined Outcomes:
mtext (expression (bold ("C.") ~ "Combined analysis"), adj = -0.37, line = -27.5)

text (c (-1.85, -1.85, -1.60, -1.60), c (-1.55, -1.65, -1.75, -1.85),
      c ("CAFS", "PROOF", "Importance", "Bothersome"),
      xpd = T, adj = 0, cex = 0.95, font = 1)

text (c (2.75, 5, 7, 8.5, 10, 11.65, 13.75), -1.40,
      c (expression (bold (bar("R")["Treated"])),
         expression (bold (bar("R")["Placebo"])),
         expression (bold (Delta * bar("R"))),
         expression (bold ("WP")),
         expression (bold ("OR")),
         expression (bold ("NNT")),
         expression (bold ("P-value"))), xpd = T,
      cex = 0.95)

segments (x0 = -1.85, y0 = -1.475, x1 = 14.85, y1 = -1.475, xpd = T)

text (t (sapply (1:3, function (i){c (2.75, 5, 7, 8.5, 10, 11.65, 13.75)})),
      sapply (1:5, function (i){c (-1.55, -1.75, -1.85)}),
      matrix (c (rank.t, rank.c, rank.diff, wp, or, nnt, pval), nrow = 3), xpd = T, cex = 0.95)


dev.off ()


