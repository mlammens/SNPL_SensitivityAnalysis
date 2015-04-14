# New approaches to global sensitivity analysis of demographic models with applications to impact assessment

This repository includes the R code used for the analysis presented in the 
manuscript with the above title, by Matthew Aiello-Lammens and 
H. Resit Akcakaya. The code in the SACode_SNPL directory is an
earlier version of the generic 
[demgsa](https://github.com/mlammens/demgsa) R package in development
by Aiello-Lammens. The purpose of this repository is to allow anyone to investigate
the methods we used in our analysis, and to repeat our analysis if desired.
To apply the global sensitivity analysis methods presented in this manuscript 
to your own RAMAS GIS models, we recommend that you use
[demgsa](https://github.com/mlammens/demgsa). 

## Replicating our analysis

Currently, the RAMAS GIS software is necessary to replicate our analysis. It can be 
replicated on either a Windows or *nix (Mac or Linux) machine, but is probably 
most straight forward on a Windows machine. Below is a brief description of the 
major scripts and their function:

### snpl_sa_main.r

This script steps through:

1. the initial global sensitivity analysis of the Snowy Plover
metapopulation model 
2. the running of all metapopulation models 
3. the extraction of the model results
4. the running of BRT models comparing input parameters to model outcomes
5. the calculation of input parameter relative influence values

### snpl_sa_development.r

This script includes code to examine difference between LHS and URS, as well as
differences in different sample sizes from parameter uncertainty space.

### snpl_sensitivity_analyses.r

This script, and the similarly named `snpl_sensitivity_analyses_ema.r`, are support
scripts for creating correlation matrices to compare the various different model
results.
