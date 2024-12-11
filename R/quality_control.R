# Quality Control Tests for BIONESS data
# Developed 2024
# Emily O'Grady, Rebecca Milne
library(tidyverse)
# These functions check the BIONESS data to be submitted to ODIS for loading to BioChem

# read in sample final file for development
data <- readxl::read_excel(path = 'R:/Science/BIODataSvc/SRC/ZPlankton_DataRescue/OGrady_2024/FINAL/HUD2014030/HUD2014030_BIONESS.xlsx')

# Formatting checks ----
# data formatting check
plankton_data_check <- function(data) {
  # detect file type and set some standard values and expectations
  # check headers
  plankton_headers <- c("MISSION" = 'character',
                        "DATE" = 'date',
                        "STN" = 'character',
                        "TOW#" = 'character',
                        "GEAR" = 'numeric',
                        "EVENT" = 'numeric',
                        "SAMPLEID" = 'numeric',
                        "START_DEPTH" = 'numeric',
                        "END_DEPTH" = 'numeric',
                        "ANALYSIS" = 'numeric',
                        "SPLIT" = 'numeric',
                        "ALIQUOT" = 'numeric',
                        "Split_fraction" = 'numeric',
                        "TAXA" = 'chr',
                        "NCODE" = 'numeric',
                        "STAGE" = 'numeric',
                        "SEX" = 'numeric',
                        "DATA_VALUE" = 'numeric',
                        "PROC_CODE" = 'numeric',
                        "What_was_it" = 'numeric',
                        "COMMENT" = 'character')
  for (i in 1:length(plankton_headers)) {
    check <- grep(pattern = names(plankton_headers)[i],x =  names(data))
    if(length(check) == 0){
      warning("Data column missing! [", plankton_headers[i], "]")
    }

  # check numeric vs character columns

    datatype <- class(data[[names(plankton_headers)[i]]])
    if (plankton_headers[i] == 'date') {
      # do a check on date formatting
    } else{
      if (datatype != plankton_headers[i]){
        warning("Data type for ", names(plankton_headers)[i], " is, ", datatype, " instead of expected, ", plankton_headers[i])
      }
    }


  # check consistency in columns that should be copied
  consistent_columns <- c("MISSION",
                          "GEAR")
  if (names(plankton_headers)[i] %in% consistent_columns) {
    if (length(unique(data[[names(plankton_headers)[i]]])) >1) {
      warning("Unexpected variation in ", names(plankton_headers)[i], " column!")
    }
  }

  }
}

# check metadata vs data
plankton_metadata_check <- function(data, metadata) {
  # check that elog file aligns with data file
  # highlight missing values from elog files
}

# Data Quality Checks ----

# Calanus analysis check
plankton_calanalysis_check <- function(data) {
  # Check that all calanus species are labelled anlaysis 2 and have the same split fraction
# pseudo code summary:
# If (taxonomic name starts with ‘calanus’, ignore case = FALSE) {
#
#   Analysis  = 2 }
#
# For each unique sample ID
#
# Calanus rows should have single unique split fraction
}


# Weight checks
plankton_weight_check <- function(data) {
  # Dry weight should always be less than half of the small wet weight for a unique sample ID
  # Total weight
  # Totwt should always equal small biomass + large biomass except where small biomass is a negative number, then totwt is that same negative

  # pseudocode summary
  # totwt is that same negative #
  #
  # If {small_Weight < 0
  #
  #   totwt = small_weight
  #
  # }Else{
  #
  #   Small_Weight + large_weight = totwt
  #
  # }
}

# Data completeness
plankton_completeness_check <- function(data) {
  # Every sample should have rows for (see Rebecca's table - sample_completeness_check.csv)
}

# Split and split fraction
plankton_split_check <- function(data) {
  # split for wet weight and large bugs is always 0, split fraction is always 1
# pseudocode summary
# Where name %in% small_wet_weight, large_wet_weight
#
# Split_fraction = 1
#
  # split for dry weight is always 1 and dry weight split fraction is always 0.5
# Where name %in% dry_weight
#
# Split_fraction = 0.5
}

# Large animal matching
plankton_lganimal_check <- function(data) {
  # large animal counts should match large animal weights

# pseudocode summary

# Large_taxa <- list(data[split_fraction == 1]![weight rows])
#
# For each large taxa
#
# should be exactly one row with count (integer)
#
# should be exactly one row with a weight (decimal, <4 precision)
}



# Numeric Precision
plankton_numeric_check <- function(data) {
  # All counts should be integers and all weights are recorded to 4 decimal places (note that excel deletes trailing zeros)
# pseudocode summary
# If (what_was_it = 1) {
#
#   data_value = integer
#
# } else{
#
#   data_value  has precision <=4
#
# }
}

