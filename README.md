# dte-ra
This repository contains R code to replicate the experimental results in ["Regression Adjustment for Estimating Distributional Treatment Effects in Randomized Controlled Trials"](https://arxiv.org/abs/2407.14074) by Tatsushi Oka, Shota Yasui, Yuta Hayakawa, and Undral Byambadalai.
 
# Getting Started
```
git clone https://github.com/CyberAgentAILab/dte-ra
```
## Installations
Install all necessary packages in R.

- R version 4.3.1
- **Packages:** `readr_2.1.4` `haven_2.5.3`        `fastglm_0.0.3`      `bigmemory_4.6.1`    `doParallel_1.0.17`  `iterators_1.0.14`  
 `foreach_1.5.2`      `foreign_0.8-84`     `patchwork_1.2.0`    `cowplot_1.1.3`      `RColorBrewer_1.1-3` `ggpubr_0.6.0`      
`gridExtra_2.3`      `ggplot2_3.5.1`      `tidyr_1.3.0`        `dplyr_1.1.2`    

## Data Preparation
1. [Ferraro & Price (2013)](https://direct.mit.edu/rest/article-abstract/95/1/64/58053/Using-Nonpecuniary-Strategies-to-Influence): Download original data from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN1/22633&version=1.1 and save `090113_TotWatDat_cor_merge_Price.dta` file in data folder.

2. [Oregon Health Insurance Experiment](https://www.nber.org/programs-projects/projects-and-centers/oregon-health-insurance-experiment?page=1&perPage=50): Download original data from https://www.nber.org/research/data/oregon-health-insurance-experiment-data and save the folder `OHIE_Public_Use_Files` in data folder.

3. Run `data/data_pre_process.R` to save the csv files named `data_ferraroprice.csv` and `data_oregon_12m.csv.`

# Analysis
`experiment` folder contains all R files used for analysis.

(1) `experiment/functions.R`: code containing all necessary functions

(2)`experiment/run_simulation.R`: code to run the Monte Carlo simulations and saves results as .rds files

(3)`experiment/compute_stats.R`: code to calculate evaluation metrics (e.g. bias, RMSE, coverage probability, average length of confidence intervals) from the saved simulation results (.rds files) and saves them as .csv files

(4)`experiment/plot_figures.R`: code to load the .csv files and plot figures for the simulation study

(5)`experiment/experiment_water_consumption.R`: code to replicate the analysis of experimental data from Ferraro & Price (2013)

(6)`experiment/experiment_oregon.R`: code to replicate the analysis of experimental data from Oregon Health Insurance Experiment

## Steps
1.  Create a folder called `result` to store all the results and figures.
2.  To replicate the results from the Monte Carlo simulation, run the files in the following order: (1) `experiment/run_simulation.R`, (2) `experiment/compute_stats.R`, (3) `experiment/plot_figures.R`.

    The output will be figures appeared in Figures 1, 2 in the main text and Figures 5-14 in the Appendix. 
4.  Run `experiment/experiment_water_consumption.R` to replicate the results from the water consumption experiment.

    The output will be figures appeared in Figure 3 in the paper.
5. Run `experiment/experiment_oregon.R` to replicate the results from the Oregon health insurance experiment.

   The output will be figures appeared in Figure 4 in the paper.
    

## Citation
```
@misc{oka2025regressionadjustmentestimatingdistributional,
      title={Regression Adjustment for Estimating Distributional Treatment Effects in Randomized Controlled Trials}, 
      author={Tatsushi Oka and Shota Yasui and Yuta Hayakawa and Undral Byambadalai},
      year={2025},
      eprint={2407.14074},
      archivePrefix={arXiv},
      primaryClass={econ.EM},
      url={https://arxiv.org/abs/2407.14074}, 
}
```
