# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 02: Bias Demonstration


## Packages
library(tidyverse)
library(lubridate)
library(texreg)


## Save dir
if (!dir.exists("results")) dir.create("results")


# Data ----

bes2017 <- readRDS("data/bes2017.rds")
bsa2017 <- readRDS("data/bsa2017.rds")




# Models and regressions ----

modela <- lrScale ~ edlevel
modelb <- alScale ~ edlevel

m1 <- lm(modela,bsa2017,weights=weight)
m2 <- lm(modela,bes2017,weights=weight)
m3 <- lm(modelb,bsa2017,weights=weight)
m4 <- lm(modelb,bes2017,weights=weight)

models <- list(m1,m2,m3,m4)

screenreg(models, custom.model.names = c("BSA Left-Right", "BES Left-Right", "BSA Lib-Auth", "BES Lib-Auth"))
saveRDS(models, file = "results/demo.rds")




# Robustness 1: BES vs BSA Membership ----

# the following matches indicators from the *recoded* datasets
# where a bes variable does not have a bsa match, the bsa side is listed as 'NA'
# there are 5 matches in total

#lr1 bes = Ordinary working people get their fair share of the nation's wealth = NA
#lr2 bes = There is one law for the rich and one for the poor = lr4 bsa
#lr5 bes = There is no need for strong trade unions to protect employees' working conditions and wages = NA
#lr6 bes = Private enterprise is the best way to solve Britain's economic problems = NA
#lr7 bes = Major public services and industries ought to be in state ownership = NA
#lr8 bes = It is the government's responsibility to provide a job for everyone who wants one = NA

#al3 bes = Young people today don't have enough respect for traditional British values = al1 bsa
#al4 bes = Censorship of films and magazines is necessary to uphold moral standards = al6 bsa
#al9 bes = People should be allowed to organise public meetings to protest against the government = NA
#al10 bes = People in Britain should be more tolerant of those who lead unconventional lives = NA
#al11 bes = For some crimes, the death penalty is the most appropriate sentence = al3 bsa
#al12 bes = People who break the law should be given stiffer sentences = al2 bsa


# summarised matchlist (bes = bsa) is:

# lr2 = lr4
# al3 = al1
# al4 = al6
# al11 = al3
# al12 = al2

temp <- bsa2017 %>%
  select(lr4, al1, al6, al3, al2, weight) %>%
  rename(lr2 = lr4,
         al3 = al1,
         al4 = al6,
         al11 = al3,
         al12 = al2) %>%
  mutate(bsa = TRUE,
         across(starts_with("al"), ~4-.x)) #agree is lowest, so flip auth to range lib to auth

both <- bes2017 %>%
  select(lr2, al3, al4, al11, al12, weight) %>%
  mutate(bsa = FALSE,
         lr2 = 4-lr2) %>% #agree is highest, so flip left statement to range left to right
  rbind(temp)


checkModel1 <- bsa ~ lr2 + al3 + al4 + al11 + al12


check1OLS <- lm(checkModel1, both, weights = weight)
check1Logit <- glm(checkModel1, both, weights = weight, family=binomial(link="logit"))
check1Probit <- glm(checkModel1, both, weights = weight, family=binomial(link="probit"))


check1 <- list(check1OLS, check1Logit, check1Probit)
screenreg(check1)

saveRDS(check1, "results/demoRobustness1.rds")





# Robustness 2: Time ----

month(bes2017$Interview_Date)

temp2 <- bes2017 %>%
  mutate(month = month(Interview_Date, label=T),
         month = factor(month, levels=c("Jul","Aug","Sep"), labels=c("Jul","Aug","Sep"), ordered=F))


check2lr <- lrScale ~ month
check2al <- alScale ~ month

check2Models <- list(check2lr, check2al)

check2 <- map(check2Models, ~lm(.x, temp2, weights=weight))
screenreg(check2)

saveRDS(check2, "results/demoRobustness2.rds")





