# Family_Affect_Systems

Note: This version of the abstract and repository are posted prior to formal peer review.

# Abstract 

Adolescence is a time period characterized by extremes in affect and increasing prevalence of mental health problems. During this time, the family remains a crucial source of both support and stress. Prior studies have illustrated how affect states of adolescents are related to interactions with parents. However, it remains unclear how affect states among family triads, that is adolescents and their parents, are related in daily life. This study investigated affect state dynamics (happy, sad, relaxed, and irritated) of 60 family triads, including 60 adolescents ($M_{age}$ = 15.92, $63.3$\% females), fathers and mothers ($M_{age} = 49.16$). The families participated in the RE-PAIR study, where they reported their affect states in four ecological momentary assessments per day for 14 days. We performed two studies. First, we used multilevel vector-autoregressive network models to estimate affect dynamics across all families, and for each family individually. Resulting models elucidated how family affect states were related at the same moment, and over time. We identified relations from parents to adolescents and vice versa, for both positive and negative affect, such as the reciprocal relation of irritation between adolescents and their parents, while considering family variation in these relations. Second, we evaluated the statistical performance of the network model via a simulation study, varying the percentage missing data, the number of families, and the number of time points. We conclude with substantive and statistical recommendations for future research on family affect dynamics. 

Link to the preregistration: https://osf.io/72c9x/registrations 

# Outline 

This repository contains all the documents (except the data) that are used to create the paper "Happy Parents, Happy Child? A Study of Family Affect Systems in Daily Life". Here, we describe the content of the repository. Please read this information carefully before using the documents. We will discuss the following folders (the other files are not relevant for the paper): 

1. Figures 
2. R code 
3. R objects 

# 1. Figures 

This folder contains all the figures that are used in the manuscript. The folder also contains the folder `Additional Networks` consisting of the networks shown in Appendix H. For the figures that are created in R, the R code is provided in the "R code" folder under "Figures". 

# 2. R code 

This folder consists of five other folders: 

1. Appendix 
2. Figures 
3. Functions 
4. Main 
5. Simulation 

We will describe each folder. 

## 2.1 Appendix 

This folder contains three other folders with R code for the respective Appendix. The folder `H. Additional networks` contains R code per subsample to estimate and visualize the networks that are shown in Appendix H. 

## 2.2 Figures 

This folder consists of R code for the figures that are shown in the paper. 

## 2.3 Functions 

This folder consists of three `.R` files. The file `famvargeneff_function.R` contains the R function for Figure 1 in the paper. The file `kalmanfilter_functions.R` contains functions for the data imputation using the Kalman filter. The file `stationaritycheck_functions.R` contains several functions to check the stationarity of the data. 

## 2.4 Main 

In this folder, there is an `.R` file with R code to estimate the family networks shown in Figure 1 based on the 60 families. 

## 2.5 Simulation 

This folder contains a `.R` file to perform the simulation explained in the paper. 

# 3. R objects 

This folder consists of three other folders: 

1. Appendix 
2. Main 
3. Simulation 

All the folders contain `.rds` files with the model fit that the results in the paper are based on. These `.rds` files can be loaded in R to use. We will discuss each folder. 

## 3.1 Appendix 

In this folder, there are three other folders for separate appendices. The folder `Additional networks` contains the model fit for each of the networks shown in Appendix H. The folder `Categorical` contains the model fit of the network that is based on the categorical data. The folder `Kalman simulation` contains the results of the simulation explained in Appendix C. 

## 3.2 Main 

This folder contains the model fit of the network shown in Figure 1 based on the 60 families. 

## 3.3 Simulation 

This folder contains two folders. The folder `Network model` contains the `.rds` file with the network model fit based on 59 families that is used for the simulation. The folder `Results` contains another folder, `Separate files`, with the results per 100 simulations (10 files) that is combined into one file called `sim_famnetwork_res_total_13042022.rds`. This file is also available as `.csv` file. Each row represents a repetition. The column `nTime` indicates how many time points are used for the specific repetition (i.e., 20, 56, or 100). The column `nind` represents the number of families used for the repetition (i.e., 30, 45, or 59) and `nmissing` the percentage of missing data (e.g., 0.25 means 25% missing data). The column `network` indicates what type of network is compared. For the simulation, we checked `temporal_thresholded` representing the nomothetic temporal network in the paper, `contemporaneous_thresholded` represeting the nomothetic contemporaneous network, `temporal_subject` representing the idiographic temporal networks, and `contemporaneous_subject` representing the idiograpic contemporaneous networks. The column `measure` specifies which comparison measure (i.e., correlation, bias, specificity, sensitivity, or precision) the row contains for which the column `value` contains the exact value of this comparison measure in the repetition. The columns `rep` and `id` contain information on the number of repetitions. 

# R Packages 

For the analyses, we used the following R packages (dependencies not included): 

- mlVAR [Epskamp, Deserno, & Bringmann, 2021; version 0.5]
- qgraph [Epskamp, Cramer, Waldorp, Schmittmann, & Borsboom, 2012; version 1.6.9]
- ggplot2 [Wickham, 2016; version 3.3.5]
- imputeTS [Moritz, Bartz-Beielstein, 2017; version 3.2]
- plyr [Wickham, 2011; version 1.8.6]
- dplyr [Wickham, François, Henry, & Müller, 2021; version 1.0.7]
- tseries [Trapletti & Hornik, 2020; version 0.10-48]
- ggbeeswarm [Clarke, Sherrill-Mix, 2017; version 0.6.0]
- gridExtra [Auguie, 2017; version 2.3]
- gridBase [Murrell, 2014; version 0.4-7]
- gridGraphics [Murrell & Wen, 2020; version 0.5-1]
- cowplot [Wilke, 2020; 1.1.1]
- parSim [Epskamp, 2020; version 0.1.4]

Also credits to the R developers: R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL: https://www.R-project.org/. Version 4.1.0.  


# Contact Information 
In case you have questions about the repository or our study, you can contact: 

Myrthe Veenman  
Leiden University  
m.veenman@fsw.leidenuniv.nl 

# Citation 
Veenman, M., Janssen, L. H. C., van Houtum, L. A. E. M., Wever, M. C. M. , Verkuil, B., Fried, E. I., and Elzinga, B. M., (2022; preprint). Happy Parents, Happy Child? A Study of Family Affect Systems in Daily Life. 

@misc{Veenman_2022H, 
 title={Happy Parents, Happy Child? A Study of Family Affect Systems in Daily Life},
 url={},
 DOI={},
 publisher={},
 author={Veenman, M., Janssen, L. H. C., van Houtum, L. A. E. M., Wever, M. C. M. , Verkuil, B., Fried, E. I., and Elzinga, B. M.},
 year={2022},
}


Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

