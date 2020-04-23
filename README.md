# Asthma_ShiftWork_UKB
Code to run the analysis for the paper "Night Shift Work Increases the Risk of Asthma"

R version 3.6.2 was used to analyse data. R packages used include; flextable (0.5.6),  lubridate (1.7.4), magrittr (1.5), officer (0.3.6), patchwork (1.0.0), RColorBrewer (1.1-2), tidyverse (1.3.0), ukbtools (0.11.0).

## load_data.R

This code is sourced by a few of the other scripts. Loads the data from the ukb_merged.RData (output from ADD FILE). Also loads in Data/shrinemedicationlist.csv and Data/occupation_asthmarisk_v2.csv and adds this data to the biobank data frame.

## table1.R

Manipulates data to create the participant characteristic table (Table 1) from the "Night Shift Work Increases the Risk of Asthma" paper. Sources load_data.R and saves output to data/table1.RData which is used in wordtables.R

## ORtables.R

Code to calculate the odds ratios tables. Sources load_data.R. Firstly defines a couple of functions. Then manipulates the data to only select the required columns and create the required fields. lastly fits the models on a variety of dependent variables. Function ORmodelrun fits models defined in the model_vec string vector, with names defined in the model_names string vector. Output is saved into data/ORtablesdata.RData which is used in wordtables.R

## indvjob_lifetime.R

Code to calculate the odds ratio tables for monthly frequency of shift work patterns (presented as a figure in the "Night Shift Work Increases the Risk of Asthma" paper - see ORplots.R). Sources load_data.R. Output is saved into data/ORtablesdata.freqNSW.RData which is used in wordtables.R

## indvjob_frequency.R

Code to calculate the odds ratio tables for lifetime shift work patterns (presented as a figure in the "Night Shift Work Increases the Risk of Asthma" paper - see ORplots.R). Sources load_data.R. Output is saved into data/ORtablesdata.sumNSW.RData which is used in wordtables.R

## chronotable_inter.R

Code to calculate the odds ratio tables for chronotype. Sources load_data.R. Output is saved into data/chronotablesdata_inter.RData which is used in wordtables.R

## ORtables_grs.R

Code to calculate the odds ratio tables for genetic risk score. Sources load_data.R and loads genetic risk scores from data/asthmaGRS_UKB.txt. Output is saved into data/ORtablesdata_grs.RData which is used in wordtables.R

## ORtable_pinteractionterms.R

Code to calculate p-values for testing the interactions between shift work and chronotype (Supplemental table 3) and shift work and GRS (Supplemental table 6) with repect to asthma risk. Output is saved into data/interactionterms.RData

## wordtables_interaction_source.R

Formats interaction tables (Supplemental tables 3 and 6). Loads data from data/ORtablesdata.RData, data/interactionterms.RData and data/ORtablesdata_grs.RData. Sourced by wordtables.R.

## wordtables.R

Formats tables. Loads data from data/table1.RData, data/ORtablesdata.RData, data/ORtablesdata.sumNSW.RData, data/ORtablesdata.freqNSW.RData, data/chronotablesdata_inter.RData, data/ORtablesdata_grs.RData and (using officer and flextable) formats them as word tables. Also sources wordtables_interaction_source.R. Saves output to tables_all.docx

## ORplots.R

Creates plots for manuscript (Figures 1-3). Loads data from data/ORtablesdata.RData data/ORtablesdata.freqNSW.RData and data/ORtablesdata.sumNSW.RData.
