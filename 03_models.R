# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 03: CFA Models


## Packages
library(tidyverse)
library(lavaan)

## Data
zeroDf <- readRDS("data/zero.rds")
empathyDf <- readRDS("data/empathy.rds")

## Functions
source("functions.R")

## Save dir
if (!dir.exists("results")) dir.create("results")



# Run models ----

## CFA1 ----

# Fit models
cfa1ZeroList <- pipe(zeroDf, identifier="Zero", type=1, ordered=F)
cfa1EmpathyList <- pipe(empathyDf, identifier="Empathy", type=1, ordered=F)

# Save
saveRDS(cfa1ZeroList, "results/cfa1ZeroList.rds")
saveRDS(cfa1EmpathyList, "results/cfa1EmpathyList.rds")


## CFA2 ----

# Fit models
cfa2ZeroList <- pipe(zeroDf, identifier="Zero", type=2, ordered=F)
cfa2EmpathyList <- pipe(empathyDf, identifier="Empathy", type=2, ordered=F)

# Save
saveRDS(cfa2ZeroList, "results/cfa2ZeroList.rds")
saveRDS(cfa2EmpathyList, "results/cfa2EmpathyList.rds")


## OCFA1 ----

# Fit models
ocfa1ZeroList <- pipe(zeroDf, identifier="Zero", type=1, ordered=T)
ocfa1EmpathyList <- pipe(empathyDf, identifier="Empathy", type=1, ordered=T)

# Save
saveRDS(ocfa1ZeroList, "results/ocfa1ZeroList.rds")
saveRDS(ocfa1EmpathyList, "results/ocfa1EmpathyList.rds")


## OCFA2 ----

# Fit models
ocfa2ZeroList <- pipe(zeroDf, identifier="Zero", type=2, ordered=T)
ocfa2EmpathyList <- pipe(empathyDf, identifier="Empathy", type=2, ordered=T)

# Save
saveRDS(ocfa2ZeroList, "results/ocfa2ZeroList.rds")
saveRDS(ocfa2EmpathyList, "results/ocfa2EmpathyList.rds")












