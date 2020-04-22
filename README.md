# Asthma_ShiftWork_UKB
Code to run the analysis for the paper "Night Shift Work Increases the Risk of Asthma"

## load_data.R

This code is sourced by a few of the other scripts. Loads the data from the ukb_merged.RData (output from ADD FILE). Also loads in Data/shrinemedicationlist.csv and Data/occupation_asthmarisk_v2.csv and adds this data to the biobank data frame.

## table1.R

Manipulates data to create the participant characteristic table (Table 1) from the "Night Shift Work Increases the Risk of Asthma" paper. Sources load_data.R and saves output to data/table1.RData which is used in wordtables.R

## ORtables.R

Code to calculate the odds ratios tables. Sources load_data.R. Firstly defines a couple of functions. Then manipulates the data to only select the required columns and create the required fields. lastly fits the models on a variety of dependent variables. Function ORmodelrun fits models defined in the model_vec string vector, with names defined in the model_names string vector. Output is saved into data/ORtablesdata.RData which is used in wordtables.R

## wordtables.R

Takes output from table1.R, ORtables.R, sumNSW, freqNSW, chronotablesdata_inter, ORtablesdata_grs and (using officer and flextable) formats them as word tables. Also sources wordtables_interaction_source.R. Saves output to tables_all.docx
