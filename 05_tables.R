# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 31: Tables to go into paper

# Packages
library(tidyverse)
library(lavaan)
library(semTable)
library(texreg)
# library(kableExtra) #redundant as far as I can tell
library(xtable)

# Data
zero <- readRDS("data/zero.rds")
empathy <- readRDS("data/empathy.rds")

# Demo Results
demoReg <- readRDS("results/demo.rds")
demoRobust1 <- readRDS("results/demoRobustness1.rds")
demoRobust2 <- readRDS("results/demoRobustness2.rds")

# Correction Results
cfa1Z <- readRDS("results/cfa1ZeroList.rds")
cfa1E <- readRDS("results/cfa1EmpathyList.rds")
cfa2Z <- readRDS("results/cfa2ZeroList.rds")
cfa2E <- readRDS("results/cfa2EmpathyList.rds")
ocfa1Z <- readRDS("results/ocfa1ZeroList.rds")
ocfa1E <- readRDS("results/ocfa1EmpathyList.rds")
ocfa2Z <- readRDS("results/ocfa2ZeroList.rds")
ocfa2E <- readRDS("results/ocfa2EmpathyList.rds")

# Correction Results Lists
resultList <- list(cfa1Z, cfa1E, cfa2Z, cfa2E, ocfa1Z, ocfa1E, ocfa2Z, ocfa2E)
zeroList <- list(cfa1Z, cfa2Z, ocfa1Z, ocfa2Z)
emList <- list(cfa1E, cfa2E, ocfa1E, ocfa2E)

# Graphing df - set up
models <- c("CFA1", "CFA2", "OCFA1", "OCFA2")
subs <- c("Zero-Sum", "Empathy")
zdf <- map(1:4, ~zeroList[[.x]]$df %>%
             mutate(model = models[.x],
                    subset = subs[1],
                    across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2
edf <- map(1:4, ~emList[[.x]]$df %>%
             mutate(model = models[.x],
                    subset = subs[2],
                    across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2

# Graphing df
graphdf <- rbind(zdf,edf)

# second graphing df
zdf2 <- map(c(1,3), ~zeroList[[.x]]$df %>%
              mutate(model = c("CFA", "", "OCFA")[.x],
                     subset = subs[1],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightUncorrected, AuthUncorrected, model, subset)
edf2 <- map(c(1,3), ~emList[[.x]]$df %>%
              mutate(model = c("CFA", "", "OCFA")[.x],
                     subset = subs[2],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightUncorrected, AuthUncorrected, model, subset)
graphdf2 <- rbind(zdf2,edf2)

# Regression results
nameList <- c(models[1], models)
zeroResultsLR <- readRDS("results/zeroLRreg.rds")
zeroResultsAL <- readRDS("results/zeroALreg.rds")
emResultsLR <- readRDS("results/empathyLRreg.rds")
emResultsAL <- readRDS("results/empathyALreg.rds")

# Colours
liteblu <- "#00ADEF"
darkblu <- "#222255"

# Function for CFA results
cfaOut <- function (texcode, caption="You forgot to add a caption") {
  start <- str_c("\\begin{table}[h!]\n\\centering\n\\caption{",caption,"}\n\\begin{tabular}{l c c}") #put in one c per column specified, 4 by default
  writeLines(str_c(start, str_sub(texcode, 451, str_length(texcode)), "\\end{table}"))
}

# Function for CFA results in longtable format
cfaLong <- function (texcode, caption="You forgot to add a caption") {
  start <- str_c("\\begin{longtable}[h!]{l c c}\n\\caption{",caption,"}\\\\\n") #put in one c per column specified, 4 by default
  writeLines(str_c(start, str_sub(str_sub(texcode, 451, str_length(texcode)), 1, -32), "\n\\end{longtable}"))
}

# Zero and Empathy CFA
zmod <- 'Zero =~ zero1 + zero4 + zero5 + zero7 + zero9 + zero11
         Acq =~ 1*zero1 + 1*zero4 + 1*zero5 + 1*zero7 + 1*zero9 + 1*zero11
         Zero ~~ 1*Zero
         Acq ~~ Acq'
emod <- 'Empathy =~ em1 + em2 + em3 + em4 + em5 + em6 + em7 + em8 + em9 + em10
         Acq =~ 1*em1 + 1*em2 + 1*em3 + 1*em4 + 1*em5 + 1*em6 + 1*em7 + 1*em8 + 1*em9 + 1*em10
         Empathy ~~ 1*Empathy
         Acq ~~ Acq'
zCfa <- lavaan(zmod, zero, auto.var = T, int.ov.free = T, estimator="MLR")
eCfa <- lavaan(emod, empathy, auto.var = T, int.ov.free = T, estimator="MLR")



# Demo ----

## Main Results ----

texreg(demoReg,
       file="tables_demoReg.tex",
       custom.model.names = c("BSA Left-Right", "BES Left-Right", "BSA Lib-Auth", "BES Lib-Auth"),
       custom.coef.names = c("Intercept", "GCSE/Equiv", "A-level/Equiv", "Undergrad", "Postgrad"),
       stars=NULL,
       float.pos="h!",
       label = "table:demoReg",
       caption = "BSA and BES Scales Regressed on Education",
       caption.above = T)

## Robustness ----

texreg(demoRobust1,
       file="tables_demoRobust1.tex",
       custom.model.names = c("OLS", "Logit", "Probit"),
       custom.coef.names = c("Intercept", map_chr(1:5, ~paste0("Ind",.x))),
       stars=NULL,
       float.pos="h!",
       label = "table:demoRobust1",
       caption = "Regression of Survey Membership on Common Indicators",
       caption.above = T)

texreg(demoRobust2,
       file="tables_demoRobust2.tex",
       custom.model.names = c("Left-Right", "Lib-Auth"),
       custom.coef.names = c("Intercept", "Aug", "Sep"),
       stars=NULL,
       float.pos="h!",
       label = "table:demoRobust2",
       caption = "Regression of Scales on Survey Month",
       caption.above = T)



# Correction CFA ----

# Scale Check CFA ----

semTable(zCfa,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "variances"="Latent Variances"),
         label = "table:zeroCFACheck") %>%
  cfaOut(caption="Zero CFA Check") %>%
  capture.output() %>%
  writeLines(con="tables_zCFA.tex", sep="\n")

semTable(eCfa,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:emCFACheck") %>%
  cfaOut(caption="Empathy CFA Check") %>%
  capture.output() %>%
  writeLines(con="tables_eCFA.tex", sep="\n")




# CFA Results ----

## Zero CFA ----

semTable(zeroList[[1]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:zeroCFA1") %>%
  cfaLong(caption = "Zero-Sum CFA1") %>%
  capture.output() %>%
  writeLines(con="tables_zeroCFA1.tex", sep="\n")

semTable(zeroList[[2]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:zeroCFA2") %>%
  cfaLong(caption = "Zero-Sum CFA2") %>%
  capture.output() %>%
  writeLines(con="tables_zeroCFA2.tex", sep="\n")

semTable(zeroList[[3]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:zeroOCFA1") %>%
  cfaLong(caption = "Zero-Sum OCFA1") %>%
  capture.output() %>%
  writeLines(con="tables_zeroOCFA1.tex", sep="\n")

semTable(zeroList[[4]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:zeroOCFA2") %>%
  cfaLong(caption = "Zero-Sum OCFA2") %>%
  capture.output() %>%
  writeLines(con="tables_zeroOCFA2.tex", sep="\n")


## Empathy CFA ----

semTable(emList[[1]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:emCFA1") %>%
  cfaLong(caption = "Empathy CFA1") %>%
  capture.output() %>%
  writeLines(con="tables_emCFA1.tex", sep="\n")

semTable(emList[[2]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:emCFA2") %>%
  cfaLong(caption = "Empathy CFA2") %>%
  capture.output() %>%
  writeLines(con="tables_emCFA2.tex", sep="\n")

semTable(emList[[3]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:emOCFA1") %>%
  cfaLong(caption = "Empathy OCFA1") %>%
  capture.output() %>%
  writeLines(con="tables_emOCFA1.tex", sep="\n")

semTable(emList[[4]]$fit,
         print.results = F,
         columns = c(est = "Estimate",
                     se = "SE"),
         paramSets = c("loadings", "latentvariances"),
         paramSetLabels = c("loadings" = "Loadings", "latentvariances"="Latent Variances"),
         label = "table:emOCFA2") %>%
  cfaLong(caption = "Empathy OCFA2") %>%
  capture.output() %>%
  writeLines(con="tables_emOCFA2.tex", sep="\n")




# Correction Correlations ----

# Prepare data
groups <- graphdf %>%
  group_by(subset, model) %>%
  group_split()

# Get scales
zlr <- map_dfc(5:8, ~groups[[.x]]$RightCorrected)
elr <- map_dfc(1:4, ~groups[[.x]]$RightCorrected)
zla <- map_dfc(5:8, ~groups[[.x]]$AuthCorrected)
ela <- map_dfc(1:4, ~groups[[.x]]$AuthCorrected)

# Build list of dataframes
cordfs <- list(zlr, elr, zla, ela)

# Name the columns
for (i in 1:4) {

  names(cordfs[[i]]) <- c("CFA1", "OCFA1", "CFA2", "OCFA2")

}

# Loop over, build correlation matrix
cors <- map(cordfs, cor)

# Build tables
ctabs <- map(cors, function(x) {
  mat <- x %>% round(3)
  index <- lower.tri(mat)
  mat[!index] <- ""
  return(mat)
})

# Convert to latex
captions <- c("Zero-Sum Left-Right", "Empathy Left-Right", "Zero-Sum Lib-Auth", "Empathy Lib-Auth")
ctabstex <- map(1:length(ctabs), ~xtable(ctabs[[.x]], captions[.x]))

# Save
saveCor <- function(x, file) {
  print(ctabstex[[x]], caption.placement="top") %>%
    capture.output() %>%
    writeLines(file, sep="\n")
}
f <- str_c("tables_", c("zeroLRCor", "zeroALCor", "emLRCor", "emALcor"), ".tex")
map(1:4, ~saveCor(.x, f[.x]))



# Correction Regs ----

nameList2 <- nameList
nameList2[1] <- "Raw"
coefs <- c("Intercept", "Below GCSE", "GCSE/Equiv", "A-level/Equiv", "Undergrad", "Postgrad")

texreg(zeroResultsLR,
       file="tables_zeroLR.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Zero-Sum Left-Right",
       stars=NULL,
       label = "table:zeroLR",
       float.pos = "h!")

texreg(emResultsLR,
       file="tables_emLR.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Empathy Left-Right",
       stars=NULL,
       label = "table:emLR",
       float.pos = "h!")

texreg(zeroResultsAL,
       file="tables_zeroAL.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Zero-Sum Libertarian-Authoritarian",
       stars=NULL,
       label = "table:zeroAL",
       float.pos = "h!")

texreg(emResultsAL,
       file="tables_emAL.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Empathy Libertarian-Authoritarian",
       stars=NULL,
       label = "table:emAL",
       float.pos = "h!")


## Robustness Regressions ----

### Regs ----

# Left-Right
lrComp <- lrScale ~ edlevel
lrModel <- RightCorrected ~ edlevel

# Auth-Lib
alComp <- alScale ~ edlevel
alModel <- AuthCorrected ~ edlevel

# Models
lrModels <- c(lrComp, replicate(4, lrModel))
alModels <- c(alComp, replicate(4, alModel))

# Names - namelist in regs above
coefs <- c("Intercept", "GCSE/Equiv", "A-level/Equiv", "Undergrad", "Postgrad")

# Run regressions
zeroResultsLRAlt <- map(1:5, ~lm(lrModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsLRAlt <- map(1:5, ~lm(lrModels[[.x]], edf %>% filter(model == nameList[.x])))
zeroResultsALAlt <- map(1:5, ~lm(alModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsALAlt <- map(1:5, ~lm(alModels[[.x]], edf %>% filter(model == nameList[.x])))



### Tables ----

texreg(zeroResultsLRAlt,
       file="tables_zeroLRAlt.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Zero-Sum Left-Right Alternative",
       stars=NULL,
       label = "table:zeroLRAlt",
       float.pos = "h!")

texreg(emResultsLRAlt,
       file="tables_emLRAlt.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Empathy Left-Right Alternative",
       stars=NULL,
       label = "table:emLRAlt",
       float.pos = "h!")

texreg(zeroResultsALAlt,
       file="tables_zeroALAlt.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Zero-Sum Libertarian-Authoritarian Alternative",
       stars=NULL,
       label = "table:zeroALAlt",
       float.pos = "h!")

texreg(emResultsALAlt,
       file="tables_emALAlt.tex",
       custom.model.names = nameList2,
       custom.coef.names = coefs,
       caption.above = T,
       caption = "Empathy Libertarian-Authoritarian Alternative",
       stars=NULL,
       label = "table:emALAlt",
       float.pos = "h!")





# Extra Table for Reviewer 2 ----

# Zero df
zdf2 <- map(1:4, ~zeroList[[.x]]$df %>%
              mutate(model = models[.x],
                     subset = subs[1],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind)

# Empathy df
edf2 <- map(1:4, ~emList[[.x]]$df %>%
              mutate(model = models[.x],
                     subset = subs[2],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind)

## Acquiescence Index ----
acqcor <- rbind(
  data.frame(
    Subset = "Zero-Sum",
    Model = models,
    Correlation = zdf2 %>%
      mutate(count = zero7 + zero11 + zero5 + zero9 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$count) %>% round(2)
      })
  ),
  data.frame(
    Subset = "Empathy",
    Model = models,
    Correlation = edf2 %>%
      mutate(count = em1 + em2 + em3 + em4 + em5 + em6 + em7 + em8 + em9 + em10 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$count) %>% round(2)
      })
  )
)

xtable(acqcor, caption="Correlations of Acquiescence Factor with Additive Agreement") %>%
  print(caption.placement="top",
        table.placement="h!",
        include.rownames=F) %>%
  capture.output() %>%
  writeLines("tables_acqcor.tex", sep="\n")



## LR Scale ----
rbind(
  data.frame(
    `Sub-Sample` = "Zero-Sum",
    Model = models,
    Correlation = zdf2 %>%
      mutate(count = zero7 + zero11 + zero5 + zero9 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$lrScale) %>% round(2)
      })
  ),
  data.frame(
    `Sub-Sample` = "Empathy",
    Model = models,
    Correlation = edf2 %>%
      mutate(count = em1 + em2 + em3 + em4 + em5 + em6 + em7 + em8 + em9 + em10 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$lrScale) %>% round(2)
      })
  )
)


## AL Scale ----
rbind(
  data.frame(
    `Sub-Sample` = "Zero-Sum",
    Model = models,
    Correlation = zdf2 %>%
      mutate(count = zero7 + zero11 + zero5 + zero9 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$alScale) %>% round(2)
      })
  ),
  data.frame(
    `Sub-Sample` = "Empathy",
    Model = models,
    Correlation = edf2 %>%
      mutate(count = em1 + em2 + em3 + em4 + em5 + em6 + em7 + em8 + em9 + em10 + lr1 + lr2 + lr3 + lr4 + lr5 + al1 + al2 + al3 + al4 + al5) %>%
      group_split(model) %>%
      map_dbl(function(x) {
        cor(x$Acq, x$alScale) %>% round(2)
      })
  )
)
