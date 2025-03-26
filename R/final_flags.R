# Check final data and add 1 flags
# Emily O'Grady
# February 2025

#' Final data check and add valid flags
#'
#' @param data A zooplankton data frame following AZMP data standards
#'
#' @return
#' @export
#'
#'
final_flags <- function(data) {
  require(tidyverse)

  # Check for all column
  # check for unexpected flag values (outside 0-9)
  plankton_data_check(data)

  # Check for missing values in TAXA or DATA_VALUE
  if (sum(is.na(data$TAXA)) > 0) {
    stop('Missing values in TAXA column')
  }
  if (sum(is.na(data$DATA_VALUE)) > 0) {
    stop('Missing values in DATA_VALUE column')
  }

  # fill in missing flags with 1
  data <- data %>%
    mutate(DATA_QC_CODE = ifelse(is.na(DATA_QC_CODE), 1, DATA_QC_CODE))

  # format dates to prevent excel issues
  data$DATE <- as.character(data$DATE)

  # save data as excel file
  mission <- data$MISSION[1]
  filename <- paste0('BIONESS_', mission, '_final.xlsx')
  writexl::write_xlsx(data, filename)

  cat('Data has passed all checks and flags have been added. \n')
  cat('File:', filename, '\n')
}
