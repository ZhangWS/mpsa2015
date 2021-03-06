This is the code directory for "Agent-based modeling (ABM) and the Dissemination of Erroneous Information: A viral explanation of rumor propagation" 

Authors: W Zhang, L. A. Caughell and A. B. Cronkhite

##How to use this directory:

This directory contains the data that we harvested from ~15000 simulations of the NetLogo model found in the top level directory.

* RAW DATA FILES: "scalefree2.csv" and "smallworld2.csv"
	
These were custom generated by NetLogo and were very hard to read.

* DATA CLEANING FILE: "datacleaning.R" & "datareading.R"
	
This will clean and extract raw data from files in (1) into readable machine format with additional parameters needed to generate our analysis.

You MUST run datacleaning.R first and datareading.R second to generate a dataset similar to (3). To generate our analysis, run "run2processing.R", specified in (5) to generate our analysis.
	
* READABLE DATASETS: "scalefree2data.csv" and "smallworld2data.csv"
	
These files are machine readable by your statistical program of choice without breaking anything.
	
* CODEBOOK: "codebook.txt"

A simple TXT file containing the name of every variable in the readable datasets with explanations of what they are.

* DATA ANALYSIS CODE: "rún2processing.R"
	
This file will generate all tables and figures in the manuscript under review. 
