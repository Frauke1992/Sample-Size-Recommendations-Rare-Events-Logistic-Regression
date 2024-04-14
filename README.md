# Questioning-Sample-Size-Recommendations
Code for analyses for master thesis titled "Questioning Sample Size Recommendations in Rare-Events Logistic Regressions – a Simulation Study"
All data can be found at (enter Link)



Abstract


Structure

The repository is structured as follows:
- Scripts with numbers without letters are main scripts
- Scripts with letters added to the numbers are custom functions or setup scripts

There are some dependicies in the scripts
- Scripts 01 through 04 require the respective scripts with *a to be saved
- Script 06 requires script 04a
- Script 04 requires script 03 to be fully executed
- Script 05 requires script 03 to be executed until line 101, as it requires data to be loaded into "results_total" and analysis "warnings_total" to be executed.



In scripts 01 through 04 as well as 06, there is a line towards the beginning of the script that sets "directory_script". This needs to be changed to the directory of the folder containing the respective script. 
In script 03, line 28 sets "directory_data", which needs to be changed to the folder containing folder "Results Data after Model Development". 
