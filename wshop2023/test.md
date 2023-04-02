---
output:
  pdf_document: default
  html_document: default
---


```r
#Test script for workshop
```

## Initialize


```r
library(knitr)
#knitr::spin("test.r")
library(dae)
packageVersion("dae")
```

```
## [1] '3.2.15'
```

```r
library(odw)
packageVersion("odw")
```

```
## [1] '2.1.4'
```

```r
b <- 5
t <- 5
```

## Construct a systematic layout and obtain the randomized layout for an RCBD


```r
RCBD.sys <- cbind(fac.gen(list(Rows=b, Columns=t)),
                  fac.gen(generate = list(Lines = LETTERS[1:t]), times = b))
RCBD.lay <- designRandomize(allocated         = RCBD.sys["Lines"], 
                            recipient         = RCBD.sys[c("Rows", "Columns")], 
                            nested.recipients = list(Columns = "Rows"),
                            seed = 1134)
```

## Plot the layout


```r
designGGPlot(RCBD.lay, labels = "Lines", cellalpha = 0.75,
             axis.text.size = 20, size = 8, 
             blockdefinition = cbind(1,t))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## Get the anatomy of the layout


```r
RCBD.canon <- designAnatomy(formulae = list(plots = ~ Rows/Columns, 
                                            lines = ~ Lines),
                            data = RCBD.lay)
summary(RCBD.canon)
```

```
## 
## 
## Summary table of the decomposition for plots & lines
## 
##  Source.plots  df1 Source.lines df2 aefficiency eefficiency order
##  Rows            4                                               
##  Columns[Rows]  20 Lines          4      1.0000      1.0000     1
##                    Residual      16
```

## Use odw to get an optimal row-column design


```r
RC.odw <- odw (fixed = ~ Rows + Columns,
               residual = ~ Rows:Columns,
               permute = ~ Lines, maxit = 500,
               data = RCBD.lay)
```

```
## Mon Mar 20 21:09:06 2023
## Initial criterion = 0.496091 (5 A-equations; rank C 4)
## Final criterion after 500 iterations: 0.419048
## Cleaning up: Mon Mar 20 21:09:06 2023
```

```r
RC.lay <- RC.odw$design
```

## Plot the layout


```r
designGGPlot(RC.lay, labels = "Lines", cellalpha = 0.75,
             axis.text.size = 20, size = 8)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

