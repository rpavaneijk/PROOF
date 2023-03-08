## Figure 4 manuscript ##
library (alluvial)

pdf (file = "/Users/rpavaneijk/SurfDrive/Writing/Artikelen/ACTIVE/1. PROOF Sequel/Figures/FIG3c.pdf", width = 7, height = 8)

par (fig = c (0.085,0.7,0.5,.95))
alluvial (
  plt,
  freq = as.numeric (tab.cfb),
  col = ifelse (plt$T2 == "Died", "grey50",
                ifelse (plt$T2 == "Quartile 1", cols (4)[1], 
                        ifelse (plt$T2 == "Quartile 2", cols (4)[2],
                                ifelse (plt$T2 == "Quartile 3", cols (4)[3], cols (4)[4])))),
  border = ifelse (plt$T2 == "Died", adjustcolor(1, .5),
                   ifelse (plt$T2 == "Quartile 1", cols (4)[1], 
                           ifelse (plt$T2 == "Quartile 2", cols (4)[2],
                                   ifelse (plt$T2 == "Quartile 3", cols (4)[3], cols (4)[4])))),
  alpha = 0.2,
  hide = as.numeric (tab.cfb) < 5,
  blocks = T,
  gap.width	= 0.1,
  xw = 0.2, # curviness
  cw = 0.20, # width of box
  axis_labels = c (expression (bold ("Baseline")),
                   expression (bold ("Month 12"))),
  cex = 0.9
)

mtext (expression (bold ("A.") ~ "Change from baseline"),
       line = 3.75, adj = 0, font = 2, cex = 1.15)

par (fig = c (0,1,0.5,.96), new = T, mar = c (0,0,0,0))
plot (NULL, ylim = c (0,1), xlim = c (0,1), axes = F)

text (0.025, 0.87, "Higher\nranks", font = 1)
text (0.025, 0.19, "Lower\nranks", font = 1)

arrows (x0 = 0.025, y0 = 0.77, x1 = 0.025, y1 = 0.29, code = 3, length = 0.1)
arrows (x0 = 0.845, y0 = 0.90, x1 = 0.845, y1 = 0.34, code = 3, length = 0.05, angle = 90)
arrows (x0 = 0.845, y0 = 0.22, x1 = 0.845, y1 = 0.115, code = 3, length = 0.05, angle = 90)
text (0.86, mean (c (0.96, 0.29)), "Survivors", adj = 0, font = 2)
text (0.86, mean (c (0.26, 0.075)), "Deaths", adj = 0, font = 2)

arrows (x0 = 0.27, y0 = -0.01, x1 = 0.5, y1 = -0.01, code = 2, length = 0.1, xpd = T)
text (0.7, c (mean (c (0.27, 0.075)), 0.37, 0.53, 0.7, 0.87), c ("1 to 101",
                       "102 to 184",
                       "185 to 266",
                       "267 to 348",
                       "348 to 430"), font = 3, xpd = T, adj = 0)


## Figure B
par (fig = c (0.085,0.7,0,.45), new = T)
alluvial (
  plt,
  freq = as.numeric (tab.fu),
  col = ifelse (plt$T2 == "Died", "grey50",
                ifelse (plt$T2 == "Quartile 1", cols (4)[1], 
                        ifelse (plt$T2 == "Quartile 2", cols (4)[2],
                                ifelse (plt$T2 == "Quartile 3", cols (4)[3], cols (4)[4])))),
  border = ifelse (plt$T2 == "Died", adjustcolor(1, .5),
                   ifelse (plt$T2 == "Quartile 1", cols (4)[1], 
                           ifelse (plt$T2 == "Quartile 2", cols (4)[2],
                                   ifelse (plt$T2 == "Quartile 3", cols (4)[3], cols (4)[4])))),
  alpha = 0.2,
  hide = as.numeric (tab.fu) < 5,
  blocks = T,
  gap.width	= 0.1,
  xw = 0.2, # curviness
  cw = 0.20, # width of box
  axis_labels = c (expression (bold ("Baseline")),
                   expression (bold ("Month 12"))),
  cex = 0.9
)

mtext (expression (bold ("B.") ~ "Observed score"),
       line = -0.25, adj = 0, font = 2, cex = 1.15)

par (fig = c (0,1,0,.46), new = T, mar = c (0,0,0,0))
plot (NULL, ylim = c (0,1), xlim = c (0,1), axes = F)

text (0.025, 0.87, "Higher\nranks", font = 1)
text (0.025, 0.19, "Lower\nranks", font = 1)

arrows (x0 = 0.025, y0 = 0.77, x1 = 0.025, y1 = 0.29, code = 3, length = 0.1)
arrows (x0 = 0.845, y0 = 0.90, x1 = 0.845, y1 = 0.34, code = 3, length = 0.05, angle = 90)
arrows (x0 = 0.845, y0 = 0.22, x1 = 0.845, y1 = 0.115, code = 3, length = 0.05, angle = 90)
text (0.86, mean (c (0.96, 0.29)), "Survivors", adj = 0, font = 2)
text (0.86, mean (c (0.26, 0.075)), "Deaths", adj = 0, font = 2)

arrows (x0 = 0.27, y0 = -0.01, x1 = 0.5, y1 = -0.01, code = 2, length = 0.1, xpd = T)
text (0.7, c (mean (c (0.27, 0.075)), 0.37, 0.53, 0.7, 0.87), c ("1 to 101",
                                                                 "102 to 184",
                                                                 "185 to 266",
                                                                 "267 to 348",
                                                                 "348 to 430"), font = 3, xpd = T, adj = 0)



dev.off ()


