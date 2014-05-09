# PREDATA

- **Author:** Guillaume T. Vallet, gtvallet@gmail.com, Université de Montréal, CRIUGM
- **Version:** 0.1
- **Date:** 2014/05/08

*Predata* is R package developed for a personal use to improve data pre-analysis.
*Predata* offers a set of functions to get and merge csv files (e.g. raw data from an experiment) and compute some descriptive statistics including standard errors correct for within-subject design.
These latter functions were adapted from the scripts presented on the [Cookbook](http://www.cookbook-r.com/Manipulating_data/Summarizing_data/) website. 

The detailed description of the functions as well as examples are provided in the R documentation. 
Type ``?*function_name*`` in your R console to access it.


## Table of Contents

- [Licence](#licence)
- [Dependencies](#dependencies)
- [Installation](#install)
- [Functions](#functions)


## <a name='licence'></a>Licence

This package is released under the [Creative Common Attribution-NonCommercial-ShareAlike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/) license.


## <a name='dependencies'></a>Dependencies

*Predata* depends on the ``plyr`` package. 
You can install *plyr* by typing ``install.packages('plyr')`` in your R console.


## <a name='install'></a>Installation

To install a R package from Github, you first need to install the devtools package.
In R, type ``install.packages('devtools')``. 
Then install *predata* with the following command : ``install_github('cogitos/predata')``.
And now enjoy the package!


## <a name='functions'></a>Functions

### getDataFiles

This function is designed to grab all csv files in a given folder and merge their data into a unique data frame. 
The options allow to specify if some row should be skipped, how many rows and which columns should be read in addition of the column separator.

### reportSE & reportWithin

These functions compute descriptive statistic as the summary or the describe (``psych package``) functions but they also report the standard error and the confident interval. 
The main interest of these functions is to allow to correct the standard errors for the within-subject design.
*Reportwithin* rely on the *reportSE* function and correct for within-subject design.