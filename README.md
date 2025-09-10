# Dynamic Prediction in Mixture Cure Models
This repository contains the code and material to reproduce the results of the paper:
"Dynamic Prediction in Mixture Cure Models: A Model-Based Landmarking Approach"

The project is organized into two main directories:

- R_Simulations: code for the simulation study
- R_Analysis: code for the analysis of the renal dataset


# R_Simulations
This directory includes all the code used to run the simulation studies. The key files are:
- `functions.R` —> contains all the necessary functions and the main implementation of the method described in Section 3 of the paper.
- `parameters.R` —> defines the parameter settings for each simulation scenario.
- `simulation.R` —> script for running the simulations in parallel, making use of the functions defined in `functions.R`.


# R_Analysis
This directory contains the code used to analyze the `renal` dataset, available in the `joinRML` R package. 
