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

## Known issues
- out of memory in parallel evaluation, especially for large samples and many noise variables (potentially do not read in all samples at once, but if you do this, you need to change how the generated samples are stored)
- output of logfile not fully helpful due to parallelization
- marginal vs. conditional probability  (intercept for lower marginal prev: -7.373546)
- how can we reliably detect separation? (must already be changed in 02a -> line 373 (this is not necessarily separation! predicted probabilities close to 1 are not the same as separation) possibly, this needs to be changed in the glm-fitting function already
- 02a: changing the marginal probability will create new errors because some metrics cannot be computed and there are often less than 8 observations with class = 1
- in 03a: adapt how errrors are counted in the new version
- 03a: predictors receive NA either when they are non-significant OR if log Reg has failed (then all are NA)
- labels in figures need to be adapted when the conditions are changed

## To-DO @Florian 
- detect separation by fitted probs 0/1 AND invalid standard errors
- Effektstärken an Kristins Metaanalyse verankern
- anvisierte marginal probs: 0.5, 0.1, 0.05, 0.01, 0.005
- do a test run and check the separation criterion, is this criterion too conservative and does it behave better in large samples?
- was ist mit den Falsch-Positiven bei der LogReg, ist uns das egal?

