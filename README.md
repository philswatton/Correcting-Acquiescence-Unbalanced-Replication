# README

This repository contains the replication code for my paper "*Agree to Agree: Correcting Acquiescence Bias in Fully Unbalanced Scales with Application to UK Measurements of Political Beliefs*".

A warning - the code in this repository is some the earliest from my PhD, and is much messier than I would produce today. I have tried to clean it up somewhat and succeeded in the model-running code, but the code for producing outputs (both plots and tables) remains a mess as I ran out of energy for cleaning it up. I hope that this does not detract from its usefulness.

## Scripts

In general, the scripts below should be run sequentially, starting from script 02 (see note on data below for why you will need to skip script 01 without some additional work). The scripts contained in this repository are as follows:

- **01_clean_data.R:** Processes raw data into the files used in the rest of this repository (see note below on data)
- **02_demonstration.R:** Code to replicate the demonstration of acquiescence bias from the paper, not including plots or tables.
- **03_models.R:** Code to replicate the correction of acquiescence bias in the BESIP data, not including plots or tables.
- **04_outputs.R:** Code to replicate the plots from both sections of the paper, including appendix material.
- **05_tables.R:** Code to replicate the tables from both sections of the paper, including appendix material.
- **06_models_3_cat.R:** Code to replicate the robustness check using 3-category versions of the data.

Additionally, the repository contains:

- **functions.R:** functions used in script 03
- **utils.R:** functions used in plotting


## Data

### Raw data

I have included the outputs from script 01 in the data folder of this repostiory, but not the original datasets. The original datasets used in script 01 are:

- **BES2019_W20_Panel_v0.1.dta:** BESIP combined waves panel dataset. Downloaded from <https://www.britishelectionstudy.com/data-objects/panel-study-data/>. Note this was an older version of the dataset
- **bes_f2f_2017_v1.5.dta:** BES 2017 face to face cross-sectional dataset. Downloaded from <https://www.britishelectionstudy.com/data-objects/cross-sectional-data/>
- **bsa2017_for_ukda.dta:** BSA 2017 dataset. Downloaded from <https://ukdataservice.ac.uk/>

Note that you will need to create an account on both the BES website and the UK data service in order to access the original data.

### Processed data

The processed data used in this paper is however included in the data folder. The files inside this folder are:

- **bes14.rds:** The processed data from wave 14 of BESIP
- **zero.rds:** The zero-sum subset of BESIP
- **empathy.rds:** The empathy subset of BESIP
- **bes2017.rds:** The processed BES f2f 2017 data
- **bsa2017.rds:** The processed BSA 2017 data



