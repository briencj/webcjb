#'# Script for the nonorthogonal multiphase athlete training experiment from Brien (2017)
#'## It uses a simple lattice in the first phase
#'### Initialize
library(dae)
library(knitr)
#knitr::spin("AthleticExampleLatt.v1.r")
options(width = 100)



#'## Generate a layout for Phase 1  - a nonorthogonal resolved incomplete block design 
ph1.lay <- designRandomize(allocated = data.frame(Conditions = 
                                                    factor(rep(c(1:9, 
                                                                 1,4,7,2,5,8,3,6,9, 
                                                                 1,5,9,7,2,6,4,8,3, 
                                                                 1,8,6,4,2,9,7,5,3)))),
                           recipient = list(Months = 4, Athletes = 3, Tests = 3), 
                           nested.recipients = list(Athletes = "Months", 
                                                    Tests = c("Months", "Athletes")),
                           seed = 4457)
ph1.lay <- cbind(ph1.lay, 
                 fac.divide(ph1.lay$Conditions, 
                            factor.names = list(Intensities = 3, Surfaces = 3)))
ph1.lay
#'## Check properties of the design
ph1.canon <- designAnatomy(formulae = list(tests = ~Months/Athletes/Tests, 
                                           cond = ~Intensities*Surfaces), 
                           data = ph1.lay)
summary(ph1.canon, which.criteria = c("aeff", "order"))

#'## Second phase design
#'### Phase 2 - randomize tests (and training conditions) to locations, 
#'###           but Months assigned systematicaly to Batches 
#'###           so except Batches from the randomization
ph2.lay <- designRandomize(allocated = ph1.lay,
                           recipient = list(Batches = 4, Locations = 9), 
                           nested.recipients = list(Locations = "Batches"),
                           except = "Batches", 
                           seed = 64144)
ph2.lay 

#'## Check properties of the design
ph2.canon <- designAnatomy(formulae = list(locs = ~Batches/Locations, 
                                           tests = ~Months/Athletes/Tests, 
                                           cond = ~Intensities*Surfaces), 
                           data = ph2.lay)
summary(ph2.canon, which.criteria=c("aeff", "order"))


