# Sample Size Recommendations in Rare-Events Logistic Regression
Code for analyses for master thesis titled "Questioning Sample Size Recommendations in Rare-Events Logistic Regressions – a Simulation Study"
All data can be found at (https://osf.io/8kv5j/?view_only=35dcbd5251ea4c2cb409a67a7ea83db3)



## Abstract


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
