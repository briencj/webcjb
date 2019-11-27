#'# Randomize and produce anatomy for the  athlete training examples from Brien, Harch, Correll & Bailey (2011)

#'### Intialize
library(knitr)
#knitr::spin("AthleticExamplesBHCB2011.r")
library(dae)
options(width=120)

#'## Generate a layout for Example 1 - A standard athlete training experiment
eg1.lay <- designRandomize(allocated = fac.gen(list(Conditions = 3), times=12),
                           recipient = list(Month = 4, Athlete = 3, Test = 3), 
                           nested.recipients = list(Athlete = "Month", 
                                                    Test = c("Month", "Athlete")),
                           seed = 831812)
#'### Produce the anatomy of the design
eg1.canon <- designAnatomy(formulae = list(tests = ~ Month/Athlete/Test, 
                                           cond = ~ Conditions), 
                           data=eg1.lay, which.criteria = "aeff")
summary(eg1.canon)

#'## Generate a layout for Example 2 - A simple two-phase athlete training experiment
#'### Phase 1 - randomize training conditions to test
eg2.phase1.lay <- designRandomize(allocated = fac.gen(list(Intensity = 3, Surface = 3), 
                                                      times = 4),
                                  recipient = list(Month = 4, Athlete = 3, Test = 3), 
                                  nested.recipients = list(Athlete = "Month", 
                                                           Test = c("Month", "Athlete")),
                                  seed = 2598)

#'### Phase 2 - randomize tests (and training conditions) to locations
eg2.lay <- designRandomize(allocated = eg2.phase1.lay,
                           recipient = list(Batch = 4, Location = 9), 
                           nested.recipients = list(Location = "Batch"),
                           except = "Batch", #Month is assigned systematically to Batch
                           seed = 71230)

#'### Produce the anatomy of the design
eg2.canon <- designAnatomy(formulae = list(locn = ~ Batch/Location, 
                                           tests = ~ Month/Athlete/Test, 
                                           cond = ~ Intensity*Surface), 
                           data=eg2.lay)
summary(eg2.canon, which.criteria = "aeff")

#'## Generate a layout for Example 3 - A replicated two-phase athlete training experiment
#'### Phase 1 - use layout from eg2, but expand to include Fraction
eg3.phase1.lay <- rbind(eg2.phase1.lay, eg2.phase1.lay)
#'### Randomize Fractions to Rounds
frac.lay <- designRandomize(allocated = data.frame(Fraction = factor(rep(1:2, times = 36))),
                            recipient = list(tests = 36, Round = 2),
                            nested.recipients = list(Round = "tests"),
                            seed = 64654)
frac.lay <- with(frac.lay, frac.lay[order(Round, tests),])
eg3.phase1.lay$Fraction <- frac.lay$Fraction 

#'### Phase 2 - randomize tests (and training conditions) to locations
eg3.lay <- designRandomize(allocated = eg3.phase1.lay,
                           recipient = list(Round = 2, Batch = 4, Location = 9), 
                           nested.recipients = list(Round = "Batch", 
                                                    Location = c("Batch", "Round")),
                           except = "Batch", #Month is assigned systematically to Batch
                           seed = 71230)

#'### Produce the anatomy of the design
eg3.canon <- designAnatomy(formulae = list(locn = ~ Batch/Round/Location, 
                                           tests = ~ Month/Athlete/Test/Fraction, 
                                           cond = ~ Intensity*Surface), 
                           data=eg3.lay)
summary(eg3.canon, which.criteria = "aeff")

#'## Generate a layout for Example 4 - A compensating two-phase athlete training experiment
#'### Phase 1 - randomize training conditions to test
eg4.phase1.lay <- designRandomize(allocated = fac.gen(list(Surface = 3, Intensity = 3), 
                                                      times = 4),
                                  recipient = list(Month = 4, Athlete = 3, Test = 3),
                                  nested.recipients = list(Athlete = "Month", Test = "Month"),
                                  seed = 9852)

#'### Phase 2 - randomize tests (and training conditions) to locations
eg4.lay <- designRandomize(allocated = eg4.phase1.lay,
                           recipient = list(Batch = 4, Location = 3, Period = 3), 
                           nested.recipients = list(Period = "Batch", 
                                                    Location = c("Batch", "Period")),
                           except = "Batch", #Month is assigned systematically to Batch
                           seed = 1750)

#'### Produce the anatomy of the design
eg4.canon <- designAnatomy(formulae = list(locn = ~ Batch/Period/Location, 
                                           tests = ~ Month/Athlete/Test, 
                                           cond = ~ Intensity*Surface), 
                           data=eg4.lay)
summary(eg4.canon, which.criteria = "aeff")
