# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 01: Cleaning data


## Packages
library(tidyverse)
library(haven)


# Data ----

## BESIP
besip <- read_dta("../BES2019_W20_Panel_v0.1.dta",
                  col_select = c("id","wt_new_W14", "wave14",
                                 "lr1W14W15","lr2W14W15","lr3W14W15","lr4W14W15","lr5W14W15",
                                 "al1W14W15","al2W14W15","al3W14W15","al4W14W15","al5W14W15",

                                 "zeroSum1W14","zeroSum4W14","zeroSum5W14","zeroSum7W14",
                                 "zeroSum9W14","zeroSum11W14",

                                 "empathy1W14","empathy2W14","empathy3W14","empathy4W14",
                                 "empathy5W14","empathy6W14","empathy7W14","empathy8W14",
                                 "empathy9W14","empathy10W14",

                                 "p_edlevelW14")) %>%
  filter(wave14 == 1) %>% select(-wave14)


## BES f2f
bes2017 <- read_dta("data/bes_f2f_2017_v1.5.dta") %>% filter(!is.na(wt_demog)) #some weights missing


## BSA
bsa2017 <- read_dta("data/bsa2017_for_ukda.dta")




# BESIP W14 ----

## Task:
#1) subset relevant wave & variables
#2) tidy relevant variables ready for analysis

## Notes:
# treating don't knows as missing
# coding as numeric as lavaan options allow for control over numeric vs categorical indicators


# Zero Sum:

# zeroSum7 The only way to make someone better off is to make someone else worse off +
# zeroSum1 One person’s loss is another person’s gain +
# zeroSum4 There’s only so much to go around. Life is about how big a slice of the pie you can get. +

# zeroSum11 Everyone can be a winner at the same time -
# zeroSum5 Life isn’t about winners and losers, everyone can do well -
# zeroSum9 There are ways to make everyone better off without anyone losing out -


## Empathy:

# empathy1 I can usually figure out when my friends are scared +
# empathy2 I can usually realize quickly when a friend is angry +
# empathy3 I can usually figure out when people are cheerful +
# empathy5 When someone is feeling ‘down’ I can usually understand how they feel +
# empathy6 After being with a friend who is sad about something, I usually feel sad +

# empathy4 I am not usually aware of my friends’ feelings -
# empathy7 My friends' unhappiness doesn't make me feel anything -
# empathy8 Other people's feelings don't bother me at all -
# empathy9 I don't become sad when I see other people crying -
# empathy10 My friends' emotions don’t affect me much -


## Recoding functions:

# recoding the indicators
recode_inds <- function (x, remove = 9999) {

  x <- case_when(x %in% remove ~ NA_real_,
                 TRUE ~ x - 1)
  return(x)

}

bes14 <- besip
names(bes14) <- names(bes14) %>%
  gsub("W14(W15){0,1}", "", .) %>%
  gsub("Sum", "", .) %>%
  gsub("pathy", "", .)

bes14 <- bes14 %>%

  rename(weight = wt_new_,
         edlevel = p_edlevel) %>%

  mutate(across(matches("^\\D+\\d+"), recode_inds),

         ## Scales
         lrScale = 4-(lr1 + lr2 + lr3 + lr4 + lr5)/5,
         alScale = (al1 + al2 + al3 + al4 + al5)/5,
         zeroScale = (zero1 + zero4 + (4-zero5) + zero7 + (4-zero9) + (4-zero11))/6,
         emScale = (em1 + em2 + em3 + (4-em4) + em5 + em6 + (4-em7) + (4-em8) + (4-em9) + (4-em10))/10,


         ## Left-Right and Lib-Auth Categories
         lrcat = case_when(lrScale <= 1.6 ~ "Left",
                           lrScale > 1.6 & lrScale < 2.4 ~ "Centre",
                           lrScale >= 2.4 ~ "Right"),
         lrcat = factor(lrcat, levels=c("Left","Centre","Right")),

         alcat = case_when(alScale <= 1.6 ~ "Lib",
                           alScale > 1.6 & alScale < 2.4 ~ "Centre",
                           alScale >= 2.4 ~ "Auth"),
         alcat = factor(alcat, levels=c("Lib","Centre","Auth")),


         ## Education
         edlevel2 = case_when(edlevel == 0 ~ "No qualification",
                              edlevel == 1 ~ "Below GCSE",
                              edlevel == 2 ~ "GCSE/equiv",
                              edlevel == 3 ~ "A-level/equiv",
                              edlevel == 4 ~ "Undergrad",
                              edlevel == 5 ~ "Postgrad"),
         edlevel2 = factor(edlevel2, levels=c("No qualification", "Below GCSE","GCSE/equiv","A-level/equiv","Undergrad","Postgrad")),

         edlevel = case_when(edlevel %in% 0:1 ~ "No qualification",
                             edlevel == 2 ~ "GCSE/equiv",
                             edlevel == 3 ~ "A-level/equiv",
                             edlevel == 4 ~ "Undergrad",
                             edlevel == 5 ~ "Postgrad"),
         edlevel = factor(edlevel, levels=c("No qualification","GCSE/equiv","A-level/equiv","Undergrad","Postgrad")))


## Create zero df and empathy df
zero <- bes14 %>%
  select(-starts_with("em")) %>%
  filter(!is.na(zeroScale) & !is.na(lrScale) & !is.na(alScale))

empathy <- bes14 %>%
  select(-starts_with("zero")) %>%
  filter(!is.na(emScale) & !is.na(lrScale) & !is.na(alScale))


## Save data for analysis
saveRDS(bes14, "data/bes14.rds")
saveRDS(zero, "data/zero.rds")
saveRDS(empathy, "data/empathy.rds")




# BES GE 2017 F2F ----

## recode function for bes f2f data
#1) needs to recode variables to numeric as before, but this time to construct a scale
#2) needs to reorder variables depending on which direction the statements were in


## rename, perform tasks, subset
#1) recode indicators, construct scale ranging from 0 to 4
#2) categorise respondents based on

bes2017 <- bes2017 %>%

  rename(lr1 = f01_1,
         lr5 = f01_5,
         lr6 = f01_6,
         lr2 = f01_2,
         lr7 = f01_7,
         lr8 = f01_8,
         al3 = f01_3,
         al4 = f01_4,
         al11 = f01_11,
         al12 = f01_12,
         al9 = f01_9,
         al10 = f01_10,
         weight = wt_demog) %>%

  select(finalserialno, weight, starts_with("lr"), starts_with("al"), edlevel, Interview_Date) %>%

  mutate(across(matches("^\\D+\\d+"), ~recode_inds(.x, c(-1, -999))),

         ## Scales
         lrScale = (lr1 + (4-lr2) + lr5 + lr6 + (4-lr7) + (4-lr8))/6,
         alScale = (al3 + al4 + (4-al9) + (4-al10) + al11 + al12)/6,


         ##Grouping voters
         lrcat = case_when(lrScale <= 1.6 ~ "Left",
                           lrScale > 1.6 & lrScale < 2.4 ~ "Centre",
                           lrScale >= 2.4 ~ "Right"),
         lrcat = factor(lrcat, levels=c("Left","Centre","Right")),

         alcat = case_when(alScale <= 1.6 ~ "Lib",
                           alScale > 1.6 & alScale < 2.4 ~ "Centre",
                           alScale >= 2.4 ~ "Auth"),
         alcat = factor(alcat, levels=c("Lib","Centre","Auth")),


         ##Recoding Education variable for regression later
         edlevel = case_when(edlevel == 2 ~ "GCSE/equiv",
                             edlevel == 3 ~ "A-level/equiv",
                             edlevel == 4 ~ "Undergrad",
                             edlevel == 5 ~ "Postgrad",
                             edlevel == 0 ~ "No qualification",
                             edlevel == 1 ~ "No qualification"),
         edlevel = factor(edlevel, levels=c("No qualification","GCSE/equiv","A-level/equiv","Undergrad","Postgrad")))



## export prepared data
saveRDS(bes2017, "data/bes2017.rds")




# BSA GE 2017 Data ----

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###NOTE!!!: Agree strongly here is the 'lowest' rather than 'highest' answer###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# consequentially, code reflects this - i.e. need to 'flip' auth items to put it in the right direction


# Also note the interview date variable reported in the codebook is not in the dataset:
names(bsa2017) %>% grep("Date",., T) # IntDate not included...




unique(bsa2017$redistrb)

## Subset, rename, recode
bsa2017 <- bsa2017 %>%

  rename(lr1 = redistrb,
         lr2 = BigBusnn,
         lr3 = wealth,
         lr4 = richlaw,
         lr5 = indust4,
         al1 = tradvals,
         al2 = stifsent,
         al3 = deathapp,
         al4 = obey,
         al5 = wronglaw,
         al6 = censor,
         weight = WtFactor,
         edlevel = HEdQual2) %>%

  select(Sserial, weight, starts_with("lr"), matches("^al\\d+"), edlevel) %>%

  mutate(across(matches("^\\D+\\d+"), ~recode_inds(.x, c(-1,-2,9))),

         ## Scales
         lrScale = (lr1 + lr2 + lr3 + lr4 + lr5)/5,
         alScale = 4-(al1 + al2 + al3 + al4 + al5 + al6)/6,


         ##Grouping voters
         lrcat = case_when(lrScale <= 1.6 ~ "Left",
                           lrScale > 1.6 & lrScale < 2.4 ~ "Centre",
                           lrScale >= 2.4 ~ "Right"),
         lrcat = factor(lrcat, levels=c("Left","Centre","Right")),

         alcat = case_when(alScale <= 1.6 ~ "Lib",
                           alScale > 1.6 & alScale < 2.4 ~ "Centre",
                           alScale >= 2.4 ~ "Auth"),
         alcat = factor(alcat, levels=c("Lib","Centre","Auth")),


         ##recoding education for regression later on
         edlevel = case_when(edlevel %in% 4:6 ~ "GCSE/equiv",
                             edlevel %in% 3:4 ~ "A-level/equiv",
                             edlevel == 2 ~ "Undergrad",
                             edlevel == 1 ~ "Postgrad",
                             edlevel == 8 ~ "No qualification"),
         edlevel = factor(edlevel, levels=c("No qualification","GCSE/equiv","A-level/equiv","Undergrad","Postgrad")))




##save new dta
saveRDS(bsa2017, "data/bsa2017.rds")




