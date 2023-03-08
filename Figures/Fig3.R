
pdf (file = "/Users/rpavaneijk/SurfDrive/Writing/Artikelen/ACTIVE/1. PROOF Sequel/Figures/FIG2.pdf", width = 8, height = 11)
par (mfrow = c (2,1), mar = c (10,10.25,2,1), mgp = c (2.5, 0.75, 0), cex = 0.95)
plot (km_domain, xlim = c (0,13),
      ylim = c (0, 1.025), axes = F, 
      ylab = "Overall survival (%)", 
      xlab = "", col = gg.cols (4), lwd = 1.25,
      xaxs = "i", yaxs = "i", mark = "|")
title (xlab = "Time since survey (months)", mgp = c(2.0, 1, 0))

axis (1, seq (-6,36, by = 3)); axis (2, seq (0, 1, by = 0.25), 
                                     labels = c ("0", "25", "50", "75", "100"), las = 1)

legend ("bottomleft", 
        legend = c ("Bulbar", "Fine motor", "Gross motor", "Respiratory"),
        lty = 1, col = gg.cols (4), bty = "n", lwd = 1.5, cex = 0.975)

## Mtext
text (-0.575, 1.085, "A", xpd = T, font = 2, cex = 1.25)
text (0, 1.08, "Most bothersome domain", xpd = T, font = 2, adj = 0, cex = 1.05)

## Table at risk 1:
text (-1.1, -0.165, xpd = T, "Number at risk", adj = 1, font = 2, cex = 0.95)
text (-1.1, -0.230, xpd = T, "(number censored)", adj = 1, font = 2, cex = 0.95)
text (-1.1, -0.295, xpd = T, "Bulbar", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.360, xpd = T, "Fine motor", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.425, xpd = T, "Gross motor", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.490, xpd = T, "Respiratory", adj = 1, font = 1, cex = 0.95)
# 

text (time-1.45/2.5, -0.295, evts_domain[1, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.360, evts_domain[2, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.425, evts_domain[3, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.490, evts_domain[4, ], xpd = T, cex = 0.95, adj = 0)

text (9, 0.5, xpd = T, "Log-rank P < 0.001", adj = 0.5, font = 1, cex = 0.95)


#### Part B ####
plot (km_most, xlim = c (0,13),
      ylim = c (0, 1.025), axes = F, 
      ylab = "Overall survival (%)", 
      xlab = "", col = c ("grey50", gg.cols (4)), lwd = 1.25,
      xaxs = "i", yaxs = "i", mark = "|")
title (xlab = "Time since survey (months)", mgp = c(2.0, 1, 0))

axis (1, seq (-6,36, by = 3)); axis (2, seq (0, 1, by = 0.25), 
                                     labels = c ("0", "25", "50", "75", "100"), las = 1)

legend ("bottomleft", 
        legend = c ("No preference", "Bulbar", "Fine motor", "Gross motor", "Respiratory"),
        lty = 1, col = c ("grey50", gg.cols (4)), bty = "n", lwd = 1.5, cex = 0.975)

## Mtext
text (-0.575, 1.085, "B", xpd = T, font = 2, cex = 1.25)
text (0, 1.08, "Most important domain", xpd = T, font = 2, adj = 0, cex = 1.05)

## Table at risk 1:
text (-1.1, -0.165, xpd = T, "Number at risk", adj = 1, font = 2, cex = 0.95)
text (-1.1, -0.230, xpd = T, "(number censored)", adj = 1, font = 2, cex = 0.95)
text (-1.1, -0.295, xpd = T, "No preference", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.360, xpd = T, "Bulbar", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.425, xpd = T, "Fine motor", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.490, xpd = T, "Gross motor", adj = 1, font = 1, cex = 0.95)
text (-1.1, -0.555, xpd = T, "Respiratory", adj = 1, font = 1, cex = 0.95)
# 

text (time-1.45/2.5, -0.295, evts_most[1, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.360, evts_most[2, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.425, evts_most[3, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.490, evts_most[4, ], xpd = T, cex = 0.95, adj = 0)
text (time-1.45/2.5, -0.555, evts_most[5, ], xpd = T, cex = 0.95, adj = 0)

text (9, 0.5, xpd = T, "Log-rank P = 0.036", adj = 0.5, font = 1, cex = 0.95)

dev.off ()
