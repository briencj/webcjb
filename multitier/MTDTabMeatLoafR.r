#'# Script for the two-phase meatloaf experiment
#'### Initialize
library(dae)
library(knitr)
#knitr::spin("MTDTabMeatLoafR.r")
options(width = 100)

#'## Generate a layout for Phase 1  - a randomized complete block design 
ph1.lay <- designRandomize(allocated = fac.gen(list(Rosemary = 2, Irradiation = 3), times = 3),
                           recipient = list(Blocks = 3, Meatloaves = 6), 
                           nested.recipients = list(Meatloaves = "Blocks"),
                           seed = 65431)

#'## Phase 2 design - Form systematic design for allocating meatloaves; 
#'###                 use two 6 x 6 Latin square designs in each session
ph1.sys <- data.frame(units = 1:216,
                      Blocks = factor(rep(1:3, each = 72)), 
                      Meatloaves = factor(rep(designLatinSqrSys(6), times = 6)))
ph1.sys <- merge(ph1.sys, ph1.lay)
#'### restore to Latin square order after merge reorder
ph1.sys[ph1.sys$units, ] <- ph1.sys
ph1.sys <- ph1.sys[-match("units", names(ph1.sys))]

#'## Generate a layout for Phase 2
ph2.lay <- designRandomize(allocated = ph1.sys,
                           recipient = list(Sessions = 3, Panellists = 12, TimeOrders = 6), 
                           nested.recipients = list(Panellists = "Sessions", 
                                                    TimeOrders = "Sessions"),
                           seed = 216431)

#'## Produce the anatomy of the design
ph2.canon <- designAnatomy(formulae = list(tastings = ~Sessions/(Panellists*TimeOrders), 
                                           meatloaves = ~ Blocks/Meatloaves, 
                                           treats = ~ Rosemary*Irradiation), 
                           data = ph2.lay)
summary(ph2.canon, which.criteria="none")


