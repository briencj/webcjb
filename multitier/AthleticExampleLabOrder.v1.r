#'# Script for the multiphase athlete training experiment that accounts for lab order from Brien (2017)
#'### Initialize
library(dae)
library(knitr)
#knitr::spin("AthleticExampleLabOrder.v1.r")
options(width = 100)

#'## Generate a layout for the first phase
ph1.lay <- fac.layout(unrandomized = list(Months = 4, Athletes = 3, Tests = 3), 
                      nested.factors = list(Athletes = "Months", 
                                            Tests = c("Months", "Athletes")),
                      randomized = fac.gen(list(Intensities = 3, Surfaces = 3), times = 4),
                      seed = 2598, unit.permutation=FALSE)
ph1.lay
#'## Get anatomy to check properties of the design
ph1.canon <- projs.canon(formulae = list(tests = ~Months/Athletes/Tests, 
                                         cond = ~Intensities*Surfaces), 
                         data = ph1.lay)
summary(ph1.canon, which.criteria="none")

#'## Generate systematic design proposed by RAB for Intensities and Surfaces 
#'### Uses two 3x3 mutually orthogonal Latin squares that are extended by adding 4th rows
ELS3x3 <- c(1:3, 1:3, 2,3,1, 3,1,2)
ph2.sys <- cbind(fac.gen(list(Batches = 4, Locations = 9)),
                 data.frame(Intensities = factor(rep(ELS3x3, each=3)),
                            Surfaces = factor(c(rep(c(2,3,1), 3), rep(c(3,1,2), 3), 
                                                rep(1:3, 6)))))


#'## Second phase design
#'### Generate a systematic layout for Months, Athletes, Tests, Intensities and Surfaces
ph2.sys$Months <- ph2.sys$Batches 
#now merge on commmon factors Months, Intensities and Surfaces
ph2.sys <- merge(ph1.lay, ph2.sys) 
ph2.sys  <- with(ph2.sys, ph2.sys[order(Batches,Locations),])
ph2.sys <- ph2.sys[c("Months", "Athletes", "Tests", "Intensities", "Surfaces")]

#'### Allocate the second phase
ph2.lay <- fac.layout(unrandomized = list(Batches = 4, Locations = 9),
                      randomized   = ph2.sys, 
                      except = "Batches", 
                      seed = 243526, unit.permutation=FALSE)
ph2.lay

#'### Plot the layout
A.mat <- matrix(as.character(ph2.lay$Athletes), nrow=4, byrow=TRUE)
comb.fac <- with(ph2.lay, fac.combine(list(AT = fac.combine(list(Athletes, Tests), 
                                                            combine=TRUE, sep=","), 
                                           IS = fac.combine(list(Intensities, Surfaces), 
                                                            combine=TRUE, sep=",")), 
                                      combine=TRUE, sep=" "))
comb.mat <- matrix(as.character(comb.fac), nrow=4, byrow=TRUE)
#Output the row and column labels
designPlot(A.mat,  plotlabels = FALSE, rtitle = "Batches", ctitle = "Locations", 
           rchardivisor = 6, cchardivisor = 6)
#Colour according to athlete
cell.colours <- c("lightcoral","lightcyan","lightgoldenrod")
for (i in 1:3)
  designPlot(A.mat, label = i, plotlabels = FALSE, cellfillcolour = cell.colours[i], 
             plotcellboundary = TRUE, chardivisor = 4, new=FALSE)
#Add the Athletes, Tests, Intensities, Surfaces combinations
designPlot(comb.mat, new=FALSE, chardivisor = 6)

#'## Check properties of the design
ph2.canon <- projs.canon(formulae = list(locs = ~Batches*Locations, 
                                         tests = ~Months/Athletes/Tests, 
                                         cond = ~Intensities*Surfaces), 
                         data = ph2.lay)
summary(ph2.canon, which.criteria =c("aefficiency", "order"))
