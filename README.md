# Sample Size Recommendations in Rare-Events Logistic Regression
Code for master thesis titled "Questioning Sample Size Recommendations in Rare-Events Logistic Regressions – a Simulation Study".

All data is available on OSF (https://osf.io/8kv5j/?view_only=35dcbd5251ea4c2cb409a67a7ea83db3).



## Abstract

In binary logistic regression (LR), researchers commonly gauge the goodness of prediction models by their out-of-sample performance and focus on simple effects of predictors when exploring the necessary data volume. In this study, our aim was to examine how different dataset characteristics impact detection of true parameters and to address the research gap considering data volume requirements for interaction effects. We conducted an extensive Monte Carlo simulation, considering both simple and interaction effects of predictors. Across conditions, we varied sample size, events fraction, and the number of noise variables. Analyzing the data, we employed both conventional LR and LR with elastic net penalization (ElasticNet), adjusting tuning parameters for ElasticNet based on different measures. To address imbalanced data, we performed model development both with and without upsampling. Conventional LR showed estimation problems in numerous datasets and yielded impossible effects. In ElasticNet, we noted lower detection rates for true interaction effects compared to simple effects, as well as for small true regression weights compared to medium and large ones. It was more frequent for the final model to include all true parameters alongside some noise parameters than to recover the data generating model without additional noise parameters. Our study strongly supports the idea that interaction effects in ElasticNet necessitate larger data volumes. Moreover, we found that small effects of regression weights hinder the recovery of the data generating model and the differentiation between true and noise parameters. This phenomenon was more pronounced when considering interaction effects instead of simple effects.

## Structure

The repository is structured as follows:
- Scripts with numbers without letters are main scripts
- Scripts with *a are scripts that are used within main scripts

These are the dependencies in the scripts:
- Scripts 01 through 04 require the respective scripts with *a to be saved
- Script 06 requires script 04a
- Script 04 requires script 03 to be fully executed
- Script 05 requires script 03 to be executed until line 101, as it requires data to be loaded into "results_total" and analysis "warnings_total" to be executed.


In scripts 01 through 04 as well as 06, there is an object called "directory_script" in either line 7 (scripts 01 and 02) or line 11 (scripts 03, 04 and 06).
The directory of the folder containing the respective script needs to be added here.

In script 03, there is an object called "directory_data" in line 28. The directory of the folder containing the results folder "Results Data after Model Development" needs to be added here. 
