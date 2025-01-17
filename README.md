
# BIONESSQC

<!-- badges: start -->
<!-- badges: end -->

The goal of BIONESSQC is to produce quality control for BIONESS plankton data. 
This package was designed as part of a data rescue project at Bedford Institute 
of Oceanography to archive BIONESS data collected between 2009 and 2016. 

## Installation

You can install the development version of BIONESSQC like so:

``` r
install_github('EOGrady21/BIONESSQC')
```

This QC package is designed to work with processed BIONESS data in the format 
found in the example file in `data`. The file `QC_Workflow.Rmd` lays out the 
workflow for applying QC to the data and exporting results. 

## Example

```{r}
library(BIONESSQC)
library(tidyverse)
library(readxl)
library(here)

dir.create(here('QC_output'), showWarnings = FALSE)

# read in data
  data <- read_xlsx(here("data/HUD2013037_BIONESS.xlsx"), sheet = 1)
  metadata <- read_excel(here("data/HUD2013037_InputTbl_Elog.xlsx"), sheet = 1)

  
# open output document named with mission name from data file
mission <- "HUD2013037"
output_file <- paste0(mission, "_QC_", Sys.Date(), ".txt")
sink(file.path(here('QC_output'), output_file))

# write header
cat(paste0("Quality Control Report for ", mission, "\n"))
cat(paste0("Date: ", Sys.Date(), '   ', format(Sys.time(), '%H:%M:%S'), "\n\n"))
cat("---------------------------------------------\n\n")


# run QC tests on each file
plankton_data_check(data)

plankton_metadata_check(data, metadata)

plankton_completeness_check(data)

plankton_calanalysis_check(data)

plankton_lganimal_check(data)

plankton_numeric_check(data)

plankton_split_check(data)

plankton_weight_check(data)

cat("All checks complete.\n")

sink()


```
