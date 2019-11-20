Brief introduction to R for Epidemiologists
================
20 November 2019

<!-- README.md is generated from README.Rmd. Please edit that file -->

### Background:

This repository is for the development and maintenance of R teaching
material for use in the EPIET (European Programme of Intervention
Epidemiology Training) and UK FETP (United Kingdom Field Epidemiology
Training Programme) **introductory course (IC)**.

The module now includes a brief demonstration session to introduce
participants to R.

An R markdown document demonstrates how to perform key tasks for
epidemiologists:

  - Setting up an R project file in RStudio
  - Create an R script
  - Create an R markdown / notebook file
  - Installing and loading packages
  - Setting the working directory using the `here` package
  - Importing and viewing a dataset
  - Exploring an imported dataset
  - Recoding variables
  - Creating new variables with conditional logic (e.g. a case
    definition)
  - Performing descriptive analysis (epicurve of symptom onset dates and
    age sex profile of cases)
  - Calculating risk ratios for exposures (raw and stratified)

### Acknowledgements:

This material is based on an equivalent introductory guide to STATA for
epidemiologists, written by Alicia Barrasa, for the EPIET/EUPHEM
programme introductory course. The document demonstrates how to perform
the same steps in R.

### Requirements:

This demonstration uses a STATA `.dta` teaching dataset from an outbreak
of gastrointestinal infection that occured at a school dinner. The
dataset is included in this repository.

To run this demonstration, the demonstrator will need the following
software installed on their machine:

  - R (download and install the latest version from CRAN
    [here](https://cran.r-project.org/))
  - RStudio (download the latest version as an installer or ready-to-use
    `.zip` file [here](https://rstudio.com/products/rstudio/download/))
  - Rtools (download and install the latest version from CRAN
    [here](https://cran.r-project.org/bin/windows/Rtools/))

In addition, the following packages are required (the first chunk in the
R markdown document will automatically install them if needed when run
on the demonstrator’s machine):

  - `here`: locates your working directory in the folder where you
    created your `.Rproj` file
  - `haven`: imports STATA `.dta` datasets and other less common file
    types into R
  - `data.table`: data cleaning, recoding, reshaping and creating
    summary tables
  - `lubridate`: performing calculations with dates
  - `epitrix`: create 2x2 epitables and other functions for cleaning
    epidemiological data
  - `ggplot2`: create graphs
  - `epitools`: calculate risk ratios
  - `EpiFunc`: create descriptive epidemiology figures (age sex pyramids
    and epicurves)
  - `devtools`: used to install and build the `EpiFunc` package from
    Github

### How to use:

After installing the above software, clone this repository either by
clicking on the green `Clone or download` button on this page, or by
entering the following command into git bash:

``` r

git clone https://github.com/EPIET/IntroductoryCourse.git
```

Then:

  - Open the .Rproj file in RStudio
  - Navigate to the `Files` tab within RStudio
  - Click on `20191015_Brief introduction to R_script_AM.Rmd` R markdown
    document to open it
  - Run each chunk separately in the live demonstration, explaining what
    is happening as you go.

To provide a printed copy of the demonstration R markdown for
participants:

  - Open the R markdown document in RStudio (as above)
  - Click on the `Knit` button and select `Knit to pdf`
  - This will save a .pdf version of the document in your working
    directory, which can then be printed.

### Maintenance:

This project is currently being maintained by [Amy
Mikhail](https://github.com/AmyMikhail).

Contributions are welcome: please contact the maintainer to request
access.

To report bugs or make feature requests, please post an issue
[here](https://github.com/EPIET/IntroductoryCourse/issues).
