# Acquiescence Bias Correction Paper Code ----
# Phil Swatton
# University of Essex
# File 04: Paper outputs

## Packages
library(tidyverse)
library(scales)
library(survey)
library(srvyr)
library(jtools)
library(gridExtra)
library(gtable)
library(ggpubr)
library(ggrepel)
library(ggcorrplot)
library(dotwhisker)

# Datasets
bsa <- readRDS("data/bsa2017.rds")
bes <- readRDS("data/bes2017.rds")
besip <- readRDS("data/bes14.rds")
zero <- readRDS("data/zero.rds")
empathy <- readRDS("data/empathy.rds")

# Demo Results
demoReg <- readRDS("results/demo.rds")
demoRobust1 <- readRDS("results/demoRobustness1.rds")
demoRobust2 <- readRDS("results/demoRobustness2.rds")

# Demo means
bsaMeans <- bsa %>%
  as_survey_design(weights=weight) %>%
  summarise(mean_lr = survey_mean(lrScale, na.rm=T),
            mean_al = survey_mean(alScale, na.rm=T))
besMeans <- bes %>%
  as_survey_design(weights=weight) %>%
  summarise(mean_lr = survey_mean(lrScale, na.rm=T),
            mean_al = survey_mean(alScale, na.rm=T))

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

# Colours
liteblu <- "#00ADEF"
darkblu <- "#222255"

# Vars
figw <- figh <- 3


## Functions
source("utils.R")


## Save dir
if (!dir.exists("images")) dir.create("images")



# Demonstration Outputs ----

## Variable Distribution Plots ----

# Make each plot
bsaJoint <- build_dist_plot(bsa, "BSA")
besJoint <- build_dist_plot(bes, "BES")
besipJoint <- build_dist_plot(besip, "BESIP")

# Save
pdf("images/demoJoint.pdf", height=5, width=5*2)
demoJoint <- grid.arrange(bsaJoint, besJoint, besipJoint, ncol=3, nrow=1)
dev.off()


## Regression Coefficient Plots ----

# Make Each Plot
demop1 <- make_demo_coef_plot(demoReg, 1)
demop2 <- make_demo_coef_plot(demoReg, 2)

# Save
demoCoef <- ggarrange(demop1, demop2, ncol=2, common.legend=T, legend="right")
ggsave("images/demoCoef.pdf", demoCoef, height=4, width=4*2)



# Identification Scales ----


## Bar Plots ----

zeroBar <- ggplot(zero, aes(x=zeroScale, weight=weight)) +
  geom_bar(fill=liteblu, color="black") +
  scale_x_continuous(limits=c(0,4)) +
  scale_y_continuous(limits=c(0,1000), breaks=seq(0,1000,200)) +
  theme_bw() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5),
        plot.margin = unit(rep(0.1, 4), "lines")) +
  labs(x="Zero-Sum Likert Scale", y="Count", title="Zero-Sum")

emBar <- ggplot(empathy, aes(x=emScale, weight=weight)) +
  geom_bar(fill=liteblu, color="black") +
  scale_x_continuous(limits=c(0,4)) +
  scale_y_continuous(limits=c(0,1000), breaks=seq(0,1000,200)) +
  theme_bw() +
  theme(aspect.ratio=1,
        plot.title=element_text(hjust=0.5),
        plot.margin = unit(rep(0.1, 4), "lines")) +
  labs(x="Empathy Likert Scale", y="Count", title="Empathy")


barPlots <- ggarrange(zeroBar, emBar, ncol=2)
ggsave("images/barPlots.pdf", barPlots, height=figh, width=figw*2)



# Main Results ----

## Heatmap ----

heat <- ggplot(graphdf %>% mutate(subset=factor(subset, levels=c("Zero-Sum", "Empathy"))), aes(x=RightCorrected, y=AuthCorrected)) +
  geom_bin2d() +
  facet_grid(subset~model) +
  labs(y="Libertarian-Authoritarian Position", x="Left-Right Position") +
  scale_fill_gradientn(limits=c(0,100), breaks=seq(0,100,25), colors=c(liteblu, darkblu), name="Count") +
  geom_vline(xintercept=2, color="black") +
  geom_hline(yintercept=2, color="black") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))

ggsave("images/heat.pdf", heat, height=5, width=5*2)



## Density Plots ----

lrden <- ggplot(graphdf, aes(x=RightCorrected)) +
  geom_density(size=1, color="black", fill=liteblu) +
  facet_grid(subset~model) +
  labs(y="Density", x="Left-Right Position") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))

ggsave("images/lrden.pdf", lrden)


laden <- ggplot(graphdf, aes(x=AuthCorrected)) +
  geom_density(size=1, color="black", fill=liteblu) +
  facet_grid(subset~model) +
  labs(y="Density", x="Libertarian-Authoritarian Position") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))
ggsave("images/laden.pdf", laden)




## Correlations ----

groups <- graphdf %>%
  group_by(subset, model) %>%
  group_split()

zlr <- map_dfc(5:8, ~groups[[.x]]$RightCorrected)
elr <- map_dfc(1:4, ~groups[[.x]]$RightCorrected)
zla <- map_dfc(5:8, ~groups[[.x]]$AuthCorrected)
ela <- map_dfc(1:4, ~groups[[.x]]$AuthCorrected)

cordfs <- list(zlr, elr, zla, ela)
for (i in 1:4) {
  names(cordfs[[i]]) <- c("CFA1", "OCFA1", "CFA2", "OCFA2")
}

cors <- map(cordfs, cor)
titles <- c("Zero-Sum Left-Right", "Empathy Left-Right", "Zero-Sum Lib-Auth", "Empathy Lib-Auth")
corPlots <- map2(cors, titles, ~ggcorrplot(.x,
                                           lab=T,
                                           lab_col="white",
                                           colors=c(darkblu, "white", liteblu)) + labs(title=.y))

pdf("images/corr.pdf", height=6, width=6)
ggarrange(plotlist=corPlots,
          ncol=2,
          nrow=2,
          common.legend = T,
          legend="bottom")
dev.off()


## Coef Plots ----

### Regs ----

# Left-Right
lrComp <- lrScale ~ edlevel2
lrModel <- RightCorrected ~ edlevel2

# Auth-Lib
alComp <- alScale ~ edlevel2
alModel <- AuthCorrected ~ edlevel2

# Models
lrModels <- c(lrComp, replicate(4, lrModel))
alModels <- c(alComp, replicate(4, alModel))

# Names
nameList <- c(models[1], models)

# Run regressions
zeroResultsLR <- map(1:5, ~lm(lrModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsLR <- map(1:5, ~lm(lrModels[[.x]], edf %>% filter(model == nameList[.x])))
zeroResultsAL <- map(1:5, ~lm(alModels[[.x]], zdf %>% filter(model == nameList[.x])))
emResultsAL <- map(1:5, ~lm(alModels[[.x]], edf %>% filter(model == nameList[.x])))

# Save
saveRDS(zeroResultsLR, "results/zeroLRreg.rds")
saveRDS(zeroResultsAL, "results/zeroALreg.rds")
saveRDS(emResultsLR, "results/empathyLRreg.rds")
saveRDS(emResultsAL, "results/empathyALreg.rds")

# List
regList <- c(zeroResultsLR, zeroResultsAL, emResultsLR, emResultsAL)
dataset <- c(rep("Zero", 10), rep("Empathy", 10))
scale <- c(rep("Left-Right", 5), rep("Lib-Auth", 5), rep("Left-Right", 5), rep("Lib-Auth", 5))
model <- rep(c("Likert", "CFA1", "CFA2", "OCFA1", "OCFA2"), 4)

regdf <- map_dfr(1:20, function (x) {

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

ggsave("images/coefs.pdf", coefPlot, width=6, height=6)




## Pairwse Comparisons ----


# Calling here for sake of corrections. tidy code later
# library(GGally)

# Just make it separetely for now
zdf3 <- map(1:4, ~zeroList[[.x]]$df %>%
              mutate(model = models[.x],
                     subset = subs[1],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, Acq, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2
edf3 <- map(1:4, ~emList[[.x]]$df %>%
              mutate(model = models[.x],
                     subset = subs[2],
                     across(matches("Corrected"), ~4*((.x - min(.x))/(max(.x) - min(.x)))))) %>%
  reduce(rbind) %>%
  select(RightCorrected, AuthCorrected, Acq, model, subset, lrScale, alScale, edlevel, edlevel2) #edlevel2
graphdf3 <- rbind(zdf3,edf3)


# Corrs
groups3 <- graphdf3 %>%
  group_by(subset, model) %>%
  group_split()

# Select the vars
zlr3 <- map(5:8,
            ~groups3[[.x]] %>%
              select(`Corrected Left-Right`=RightCorrected,
                     `Likert Left-Right`=lrScale,
                     Acquiescence=Acq))
elr3 <- map(1:4,
            ~groups3[[.x]] %>%
              select(`Corrected Left-Right`=RightCorrected,
                     `Likert Left-Right`=lrScale,
                     Acquiescence=Acq))
zla3 <- map(5:8,
            ~groups3[[.x]] %>%
              select(`Corrected Lib-Auth`=AuthCorrected,
                     `Likert Lib-Auth`=lrScale,
                     Acquiescence=Acq))
ela3 <- map(1:4,
            ~groups3[[.x]] %>%
              select(`Corrected Lib-Auth`=AuthCorrected,
                     `Likert Lib-Auth`=lrScale,
                     Acquiescence=Acq))

# Name the corr dfs
cordfs3 <- list(zlr3, elr3, zla3, ela3)
for (i in 1:4) {

  names(cordfs3[[i]]) <- c("CFA1", "OCFA1", "CFA2", "OCFA2")

}

# List of correlation matrices
cor_mats <- map(1:4, ~map(cordfs3[[.x]], cor))

# Titles
titles <- c("Zero-Sum_Left-Right", "Empathy_Left-Right", "Zero-Sum_Lib-Auth", "Empathy_Lib-Auth")

# Function to build grid of 4 plots
make_corr_grid <- function(index) {
  cor_plist <- map(1:4,
                   function(x) {
                     p <- ggcorrplot(cor_mats[[index]][[x]],
                                     lab=T,
                                     lab_col="black",
                                     colors=c(darkblu, "white", liteblu)) +
                       labs(title=names(cor_mats[[index]])[x]) +
                       theme(plot.title = element_text(hjust=0.5))

                     if (x %in% 1:2) {
                       p <- p +
                         theme(axis.text.x = element_blank())
                     }

                     if (x %in% c(2,4)) {
                       p <- p +
                         theme(axis.text.y = element_blank())
                     }

                     return(p)
                   })
  out_plot <- ggarrange(plotlist=cor_plist,
                        nrow=2,
                        ncol=2,
                        align="hv",
                        common.legend=T,
                        legend="right")
  ggsave(filename=str_c("images/pair_plots_", titles[index], ".pdf"),
         width=8,
         height=8)
}

# Make the plots
map(1:4, make_corr_grid)






