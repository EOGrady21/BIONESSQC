# Quality Control Tests for BIONESS data
# Developed 2024
# Emily O'Grady, Rebecca Milne
library(tidyverse)
library(here)
# These functions check the BIONESS data to be submitted to
# ODIS for loading to BioChem

#TODO
  # flexibility for capitalization  in weights


# read in sample final file for development
# data <- readxl::read_excel(path =
#   "R:/Science/BIODataSvc/SRC/ZPlankton_DataRescue/OGrady_2024/FINAL/HUD2013004/HUD2013004_BIONESS.xlsx")
#
# metadata <- readxl::read_excel(path =
#   "R:/Science/BIODataSvc/SRC/ZPlankton_DataRescue/OGrady_2024/FINAL/HUD2013004/HUD2013004_InputTbl_Elog.xlsx")

# Formatting checks ----
#' Data formatting check
#'
#' This function checks the  formatting of the BIONESS data including column
#' names, data types, and consistency in columns that should be copied.
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
#'
plankton_data_check <- function(data) {
  w <- 0
  # detect file type and set some standard values and expectations
  # check headers
  plankton_headers <- c("MISSION" = "character",
                        "DATE" = "date",
                        "STN" = "character",
                        "TOW#" = "character",
                        "GEAR" = "numeric",
                        "EVENT" = "numeric",
                        "SAMPLEID" = "numeric",
                        "START_DEPTH" = "numeric",
                        "END_DEPTH" = "numeric",
                        "ANALYSIS" = "numeric",
                        "SPLIT" = "numeric",
                        "ALIQUOT" = "numeric",
                        "SPLIT_FRACTION" = "numeric",
                        "TAXA" = "character",
                        "NCODE" = "numeric",
                        "STAGE" = "numeric",
                        "SEX" = "numeric",
                        "DATA_VALUE" = "numeric",
                        "PROC_CODE" = "numeric",
                        "WHAT_WAS_IT" = "numeric",
                        "COMMENT" = "character")
  for (i in seq_along(plankton_headers)) {
    check <- grep(pattern = names(plankton_headers)[i], x = names(data))
    if (length(check) == 0) {
      cat("Data column missing! [", names(plankton_headers)[i], "] \n")
      w <- w+1
    }

    # check numeric vs character columns
    datatype <- class(data[[names(plankton_headers)[i]]])
    if (plankton_headers[i] == "date") {
      # do a check on date formatting
    } else {
      if (datatype != plankton_headers[i]) {
        cat("Data type for ", names(plankton_headers)[i],
          " is, ", datatype, " instead of expected, ", plankton_headers[i], '\n'
        )
        w <- w+1
      }
    }

    # check consistency in columns that should be copied
    consistent_columns <- c("MISSION", "GEAR")
    if (names(plankton_headers)[i] %in% consistent_columns) {
      if (length(unique(data[[names(plankton_headers)[i]]])) > 1) {
        cat("Unexpected variation in ",
          names(plankton_headers)[i],
          " column! \n"
        )
        w <- w+1
      }
    }
  }
  if (w == 0) {
    cat("Data formatting check passed \n")
  }
}

# check metadata vs data
#' Metadaa check
#'
#' This function checks the metadata (Elog file) against the BIONESS data to
#' ensure that all events are present in both dataframes and that the date
#' ranges match.
#'
#' @param data a BIONESS dataframe
#' @param metadata an Elog dataframe
#'
#' @return
#' @export
#'
plankton_metadata_check <- function(data, metadata) {

  w <- 0
  # Check that all events are present in both dataframes
  m_events <- unique(metadata$Event)
  d_events <- unique(data$EVENT)
  if (length(m_events) < length(d_events)) {
    cat("Events missing from metadata! \n")
    w <- w+1
  }

  # data date range
  ddate_range <- as.Date(range(data$DATE))
  #metadata date range
  mdate_range <- as.Date(range(metadata$GPSDate))

  if (any(mdate_range != ddate_range)) {
    cat("Date ranges do not match between data and metadata! \n")
    w <- w+1
  }
  if (w == 0) {
    cat("Metadata check passed \n")
  }

}


# Data Quality Checks ----

#
#' Calanus analysis check
#'
#' This function checks that all Calanus species are labelled as analysis 2 and
#' have the same split fraction for each sample id.
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
plankton_calanalysis_check <- function(data) {

  w <- 0

  # Check that all calanus species are labelled anlaysis 2 and have the same split fraction for each sample id
  cal_data <- data[grepl("^calanus", data$TAXA, ignore.case = TRUE), ]


  if (length(unique(cal_data$ANALYSIS)) > 1) {
    cat("Calanus species have different analysis values! \n")
    w <- w+1
  }

  sampleids <- unique(cal_data$SAMPLEID)
  for (si in sampleids) {
    if (length(unique(cal_data$SPLIT_FRACTION[cal_data$SAMPLEID == si])) > 1) {
      cat("Calanus species have different split fractions! \n")
      w <- w+1
    }
  }

  if (w == 0) {
    cat("Calanus analysis check passed \n")
  }

}



# Weight checks
#' Check weights
#'
#' This function checks the weight columns in the BIONESS data to ensure that
#' the dry weight is less than half of the small wet weight for a unique sample
#' ID and that the total weight is consistent with the sum of the small and large
#' biomass.
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
#'
plankton_weight_check <- function(data) {

  w <- 0
  # Dry weight should always be less than half of the small wet weight for a unique sample ID
  # Total weight
  # Totwt should always equal small biomass + large biomass except where small biomass is a negative number, then totwt is that same negative

  wt_data <- data[data$NCODE == 1608, ]

  # Dry weight should always be less than half of the small wet weight for a unique sample ID
  for (si in unique(wt_data$SAMPLEID)) {
    small_weight <- wt_data[wt_data$SAMPLEID == si & tolower(wt_data$TAXA) == "small_biomass", "DATA_VALUE"]
    dry_weight <- wt_data[wt_data$SAMPLEID == si & tolower(wt_data$TAXA) == "dry_weight", "DATA_VALUE"]
    if (dry_weight > 0.5 * small_weight) {
      cat("Dry weight is greater than half of small wet weight for sample ID ", si, '\n')
      w <- w+1
    }
  }

  large_weight <- wt_data[wt_data$SAMPLEID == si & tolower(wt_data$TAXA) == "large_biomass", "DATA_VALUE"]
  if (nrow(large_weight) == 0) {large_weight = 0}
  totwt <- wt_data[wt_data$SAMPLEID == si & tolower(wt_data$TAXA) == "totwt", "DATA_VALUE"]

  if (small_weight < 0) {
    if (totwt != small_weight) {
      cat("Total weight does not equal small biomass for sample ID ", si, '\n')
      w <- w+1
    }
  } else {
    if (totwt != small_weight + large_weight) {
      cat("Total weight does not equal small biomass + large biomass for sample ID ", si, '\n')
      w <- w+1
    }
  }
  if (w == 0) {
    cat("Weight check passed \n")
  }

}


#' Data completeness check
#'
#' This function checks the BIONESS data for completeness including the presence
#' of multiple zooplankton species in analysis 1, the presence of small biomass,
#' total weight, and dry weight, and the presence of Oithona species.
#'
#'
#' @param data
#'
#' @return
#' @export
#'
plankton_completeness_check <- function(data) {

  w <- 0

  # Check for multiple zooplankton species in analysis 1
  if (length(unique(data$TAXA[data$ANALYSIS == 1])) < 3) {
    cat("Less than 3 species detected in analysis 1! \n")
    w <- w+1
  }

  #Small Biomass check
  if (!("small_biomass" %in% tolower(data$TAXA))) {
    cat("Small biomass missing! \n")
    w <- w+1
  }
  # Small biomass should have analysis = "1" and proc_code = "22" and what_was_it = "2"
  sb_data <- data[tolower(data$TAXA) == "small_biomass", ]
  if (unique(sb_data$ANALYSIS) != 1) {
    cat("Small biomass has miscoded analysis values! \n")
    w <- w+1
  }
  if (unique(sb_data$PROC_CODE) != 22) {
    cat("Small biomass has miscoded proc_code values! \n")
    w <- w+1
  }
  if (unique(sb_data$WHAT_WAS_IT) != 2) {
    cat("Small biomass has miscoded what_was_it values! \n")
    w <- w+1
  }

  # Total weight checks
  if (!("totwt" %in% tolower(data$TAXA))) {
    cat("Total weight missing! \n")
    w <- w+1
  }
  # Totwt should have analysis = '1', proc_code = 23, and what_was_it = 2
  tw_data <- data[tolower(data$TAXA) == "totwt", ]
  if (unique(tw_data$ANALYSIS) != 1) {
    cat("Total weight has miscoded analysis values! \n")
    w <- w+1
  }
  if (unique(tw_data$PROC_CODE) != 23) {
    cat("Total weight has miscoded proc_code values! \n")
    w <- w+1
  }
  if (unique(tw_data$WHAT_WAS_IT) != 2) {
    cat("Total weight has different what_was_it values! \n")
    w <- w+1
  }

  # dry weight checks
  if (!("dry_weight" %in% tolower(data$TAXA))) {
    cat("Dry weight missing! \n")
    w <- w+1
  }
  # dry weight should have analysis = 1, proc_code = 50, and what_was_it = 3
  dw_data <- data[tolower(data$TAXA) == "dry_weight", ]
  if (unique(dw_data$ANALYSIS) != 1) {
    cat("Dry weight has miscoded analysis values! \n")
    w <- w+1
  }
  if (unique(dw_data$PROC_CODE) != 50) {
    cat("Dry weight has miscoded proc_code values! \n")
    w <- w+1
  }
  if (unique(dw_data$WHAT_WAS_IT) != 3) {
    cat("Dry weight has miscoded what_was_it values! \n")
    w <- w+1
  }

  # Oithona check
  if (!any(grepl("^Oithona", data$TAXA, ignore.case = TRUE))) {
    cat("No Oithona species detected! \n")
    w <- w+1
  }
  if (w == 0) {
    cat("Data completeness check passed \n")
  }


}


#' Split Fraction Test
#'
#' This function checks the split fraction for the BIONESS data to ensure that
#' wet weights have a split fraction of 1, dry weights have a split fraction of
#' 0.5, and that Calanus species have a single unique split fraction for each
#' sample ID.
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
#'
plankton_split_check <- function(data) {

  w <- 0
  # split for wet weight and large bugs is always 0, split fraction is always 1

  if (any(data[tolower(data$TAXA) %in% c('small_biomass', 'large_biomass'), 'SPLIT_FRACTION'] != 1)) {
    cat("Wet weights do not have split fraction of 1! \n")
    w <- w+1
  }

  # split for dry weight is always 1 and dry weight split fraction is always 0.5
  if (any(data[tolower(data$TAXA) == 'dry_weight', 'SPLIT_FRACTION'] != 0.5)) {
    cat("Dry weights do not have split fraction of 0.5! \n")
    w <- w+1
  }

# Calanus rows should have single unique split fraction for each sample id
  if (any(grepl("^calanus", data$TAXA, ignore.case = TRUE))) {
    for (si in unique(data$SAMPLEID)) {
      if (length(unique(data[data$SAMPLEID == si & data$TAXA %in% grepl("^calanus", data$TAXA, ignore.case = TRUE), 'SPLIT_FRACTION'])) > 1) {
        cat("Calanus species have different split fractions! \n")
        w <- w+1
      }
    }
  }
  if (w == 0) {
    cat("Split fraction check passed \n")
  }
}

#' Large animal check
#'
#' This function checks the large animal counts and weights to ensure that there
#' is only one count per sample ID and that the counts are integers. It also
#' checks that there is only one weight per sample ID and that the weights are
#' recorded to 4 decimal places.
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
#'
plankton_lganimal_check <- function(data) {

  w <- 0
  # large animal counts should match large animal weights

  lgt_data <- data[data$SPLIT_FRACTION == 1 & data$NCODE != 1608, ]

  lgtaxas <- unique(lgt_data$TAXA)
  for (t in lgtaxas) {
    count <- lgt_data[lgt_data$TAXA == t & lgt_data$WHAT_WAS_IT == 1, ]
    weight <- lgt_data[lgt_data$TAXA == t & lgt_data$WHAT_WAS_IT == 2,]

    # check counts
    if (length(unique(count$SAMPLEID)) != nrow(count)) {
      cat("Large taxa ", t, " has more than one count per SAMPLEID! \n")
      w <- w+1
    }
    if (any(count$DATA_VALUE %% 1 != 0)) {
      cat("Large taxa ", t, " has non-integer count! \n")
      w <- w+1
    }

    # check weights
    if (length(unique(weight$SAMPLEID)) != nrow(weight)) {
      cat("Large taxa ", t, " has more than one weight per SAMPLEID! \n")
      w <- w+1
    }
    if (any(weight$DATA_VALUE %% 1 == 0)) {
      cat("Large taxa ", t, " has integer weight! \n")
      w <- w+1
    }
  }
  if (w == 0) {
    cat("Large animal check passed \n")
  }

}




#' Numeric Precision Check
#'
#' This function checks the numeric precision of the BIONESS data to ensure that
#' all counts are integers and all weights are recorded to 4 decimal places.
#'
#'
#' @param data a BIONESS dataframe
#'
#' @return
#' @export
#'
plankton_numeric_check <- function(data) {

  # All counts should be integers and all weights are recorded to 4 decimal places (note that excel deletes trailing zeros)
  w <- 0
  if (any(data$WHAT_WAS_IT == 1 & data$DATA_VALUE %% 1 != 0)) {
    cat("Non-integer counts present! \n")
    w <- w+1
  }
  if (any(data$WHAT_WAS_IT == 2 & data$DATA_VALUE %% 1 == 0)) {
    cat("Integer weights present! \n")
    w <- w+1
  }

  if (w == 0) {
    cat("Numeric Precision check passed \n")
  }

}


