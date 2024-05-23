#Test script for workshop

#'## Initialize
library(knitr)
#knitr::spin("test.r")
library(dae)
packageVersion("dae")
library(odw)
packageVersion("odw")

b <- 5
t <- 5

#'## Construct a systematic layout and obtain the randomized layout for an RCBD
RCBD.sys <- cbind(fac.gen(list(Rows=b, Columns=t)),
                  fac.gen(generate = list(Lines = LETTERS[1:t]), times = b))
RCBD.lay <- designRandomize(allocated         = RCBD.sys["Lines"], 
                            recipient         = RCBD.sys[c("Rows", "Columns")], 
                            nested.recipients = list(Columns = "Rows"),
                            seed = 1134)

#'## Plot the layout
designGGPlot(RCBD.lay, labels = "Lines", cellalpha = 0.75,
             axis.text.size = 20, label.size = 8, 
             blockdefinition = cbind(1,t))

#'## Get the anatomy of the layout
RCBD.canon <- designAnatomy(formulae = list(plots = ~ Rows/Columns, 
                                            lines = ~ Lines),
                            data = RCBD.lay)
summary(RCBD.canon)

#'## Use odw to get an optimal row-column design
RC.odw <- odw (fixed = ~ Rows + Columns,
               residual = ~ Rows:Columns,
               permute = ~ Lines, maxit = 500,
               data = RCBD.lay)
RC.lay <- RC.odw$design

#'## Plot the layout
designGGPlot(RC.lay, labels = "Lines", cellalpha = 0.75,
             axis.text.size = 20, label.size = 8)
