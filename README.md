# Minerva project- clustering conflicts

minerva_bucketing-replication folder has everything needed to replicate the study.

data folder has raw data downloaded from PITF & PRIO website.

output folder is used to store all model outputs.


## Instruction to replicate the study findings

Order of scripts
1. Run PITF & PRIO case selection.ipynb
    1. For onset case selection from PITF & PRIO datasets, using 2-year-peace threshold
    2. Combining onset cases with IVs, using 2-year lag
2. Run Euclidean Agnes PITF_3cluster.Rmd
    1. For base model in the paper
    2. Other .Rmd files are different model settings for sensitivity analysis
3. For consolidated data visualization, use Radar Plots xxx.Rmd
4. For outputs related to sensitivity analysis in the appendix, use Output Comparison.ipynb

Software requirements
- Python 3.7.6
- R 4.1.1
