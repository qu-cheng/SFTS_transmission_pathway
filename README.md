# Assessing the relative roles of systemic, non-systemic, and transovarial transmission pathways for severe fever with thrombocytopenia syndrome virus and its implications for future research and intervention strategies


This repository contains code for reproducing the analyses in *Assessing the relative roles of systemic, non-systemic, and transovarial transmission pathways for SFTSV and its implications for future search and intervention strategies* by Cheng et al.
* **0_import_libraries.R**: R scripts for loading libraries, functions and setting values for some commonly used variables
* **1_generate_LHS_samples.R**: code for generating the 1000 random samples with Latin Hypercube sampling method under different tick abundance, duration of viremia, and tick aggregation pattern scenarios. The results were saved in the folder *Parameters*.
* **2_estimate_R0_sens_elas.R**: code for estimating the R0, sensitivity and elasticity with the NGM method. Results were saved in the *Results* folder.
* **3_fit_RF_models.R**: code for fitting random forest models to get the permutation importance and partial dependence of each model parameter. Results were saved in the *Results* folder.
* **4_Fig_main_text.R** and **5_Fig_supplementary.R**: code for regenerating the figures in the main text and supplementary materials, respectively. Results were saved in the *Figures* folder.
