# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 06: Replication with 3 categories


## Packages
library(tidyverse)
library(lavaan)

## Data
zeroDf <- readRDS("data/zero.rds")
empathyDf <- readRDS("data/empathy.rds")

## Functions
source("functions.R")
source("utils.R")



# Recode data to 3-category ----

collapse_cats <- function(x) {
  out <- case_when(x %in% 0:1 ~ 0,
                   x %in% 3:4 ~ 2,
                   T ~ 1)
  return(out)
}

zeroDf <- zeroDf %>%
  mutate(across(starts_with("lr"), collapse_cats),
         across(starts_with("al"), collapse_cats),
         across(starts_with("zero"), collapse_cats),

         lrScale = 2-(lr1 + lr2 + lr3 + lr4 + lr5)/5,
         alScale = (al1 + al2 + al3 + al4 + al5)/5,
         zeroScale = (zero1 + zero4 + (2-zero5) + zero7 + (2-zero9) + (2-zero11))/6)

empathyDf <- empathyDf %>%
  mutate(across(starts_with("lr"), collapse_cats),
         across(starts_with("al"), collapse_cats),
         across(starts_with("em"), collapse_cats),


         lrScale = 2-(lr1 + lr2 + lr3 + lr4 + lr5)/5,
         alScale = (al1 + al2 + al3 + al4 + al5)/5,
         emScale = (em1 + em2 + em3 + (2-em4) + em5 + em6 + (2-em7) + (2-em8) + (2-em9) + (2-em10))/10)


# Run Models ----

## OCFA1 ----

# Fit models
ocfa1ZeroList <- pipe(zeroDf, identifier="Zero", type=1, ordered=T)
ocfa1EmpathyList <- pipe(empathyDf, identifier="Empathy", type=1, ordered=T)


## OCFA2 ----

# Fit models
ocfa2ZeroList <- pipe(zeroDf, identifier="Zero", type=2, ordered=T)
ocfa2EmpathyList <- pipe(empathyDf, identifier="Empathy", type=2, ordered=T)



# Get Output ----

# Colours
liteblu <- "#00ADEF"
darkblu <- "#222255"

# Vars
figw <- figh <- 3

# Lists
zeroList <- list(ocfa1ZeroList, ocfa2ZeroList)
emList <- list(ocfa1EmpathyList, ocfa2EmpathyList)

# Graphing df - set up
models <- c("OCFA1", "OCFA2")
subs <- c("Zero-Sum", "Empathy")
zdf <- map(1:2, ~zeroList[[.x]]$df %>%
             mutate(model = models[.x],
                    subset = subs[1],
                    across(matches("Corrected"), ~2*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2
edf <- map(1:2, ~emList[[.x]]$df %>%
             mutate(model = models[.x],
                    subset = subs[2],
                    across(matches("Corrected"), ~2*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2

# Graphing df
graphdf <- rbind(zdf,edf)



## Heatmap ----

heat <- ggplot(graphdf %>% mutate(subset=factor(subset, levels=c("Zero-Sum", "Empathy"))), aes(x=RightCorrected, y=AuthCorrected)) +
  geom_bin2d() +
  facet_grid(subset~model) +
  labs(y="Libertarian-Authoritarian Position", x="Left-Right Position") +
  scale_fill_gradientn(limits=c(0,100), breaks=seq(0,100,25), colors=c(liteblu, darkblu), name="Count") +
  geom_vline(xintercept=1, color="black") +
  geom_hline(yintercept=1, color="black") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))

ggsave("images_heat_3_cat.pdf", heat, height=5, width=5)



## Regression ----


### Regs ----

# Left-Right
lrComp <- lrScale ~ edlevel2
lrModel <- RightCorrected ~ edlevel2

# Auth-Lib
alComp <- alScale ~ edlevel2
alModel <- AuthCorrected ~ edlevel2

# Models
lrModels <- c(lrComp, replicate(2, lrModel))
alModels <- c(alComp, replicate(2, alModel))

# Names
nameList <- c(models[1], models)

# Run regressions
zeroResultsLR <- map(1:3, ~lm(lrModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsLR <- map(1:3, ~lm(lrModels[[.x]], edf %>% filter(model == nameList[.x])))
zeroResultsAL <- map(1:3, ~lm(alModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsAL <- map(1:3, ~lm(alModels[[.x]], edf %>% filter(model == nameList[.x])))


# List
regList <- c(zeroResultsLR, zeroResultsAL, emResultsLR, emResultsAL)
dataset <- c(rep("Zero", 6), rep("Empathy", 6))
scale <- c(rep("Left-Right", 3), rep("Lib-Auth", 3), rep("Left-Right", 3), rep("Lib-Auth", 3))
model <- rep(c("Likert", "OCFA1", "OCFA2"), 4)

regdf <- map_dfr(1:12, function (x) {

  df <- broom::tidy(regList[[x]], conf.int=T)
  df$dataset <- dataset[x]
  df$scale <- scale[x]
  df$model <- model[x]
  df <- df[-1,]

  return(df)

}) %>%
  mutate(dataset = factor(dataset, levels=c("Zero", "Empathy"), labels=c("Zero-Sum", "Empathy")),
         term = factor(term, levels=c("edlevel2Below GCSE", "edlevel2GCSE/equiv", "edlevel2A-level/equiv", "edlevel2Undergrad", "edlevel2Postgrad")),
         model = factor(model, levels=c("Likert", "CFA1", "CFA2", "OCFA1", "OCFA2")))



### Plot ----

pd <- position_dodge(width = 0.4)

coefPlot <- ggplot(regdf, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, color=model, shape=model)) +
  geom_vline(xintercept=0) +
  geom_point(position=pd, size=2) +
  geom_errorbar(width=0,
                size=1,
                position=pd) +
  scale_x_continuous(limits=c(-1.5,1.5)) +
  scale_y_discrete(limits=rev, labels = c("Postrgrad", "Undergrad", "A-level/Equiv", "GSCE/Equiv", "Below GCSE")) +
  scale_color_discrete(name="Model") +
  scale_shape("Model") +
  facet_grid(dataset~scale) +
  labs(x="", y="") +
  theme_bw() +
  theme(aspect.ratio=1)

ggsave("images_coefs_3_cat.pdf", coefPlot, width=6, height=6)

