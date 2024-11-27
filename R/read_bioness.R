# READ BIONESS electronic files

#' Read BIONESS electronic files
#' Returns a BIONESS class list object with metadata represented as a list and
#' data represented as a dataframe.
#'
#' @param file
#'
#' @return a BIONESS class object with a /{metadata} and /{data} section
#' @export

read_bioness <- function(file) {
  require(tidyverse)
  require(fs)

  # tidy path and check inputs
  file <- fs::path_tidy(file)
  shortname <- str_split_i(file, pattern = '/',  i = -1)
  file_type <- NULL
  if (length(grep(shortname, pattern = '\\.T\\d{2}$')) > 0) {
    file_type <- 'T'
    #message('File type .T detected!')
  }
  if (length(grep(shortname, pattern = '\\.B\\d{2}$')) > 0) {
    file_type <- 'B'
    #message('File type .B detected!')
  }
  if(is.null(file_type)) {
    stop('File type not recognized!')
  }

  if (file_type == 'B') {
    stop('File type not supported! IN DEVELOPMENT')
  }

  # Read T files
  if (file_type == 'T') {
    linedata <- readLines(file)
  # grab net header
    netinfohead <- which(linedata == 'Net Information')
    if (length(netinfohead) == 0 | length(netinfohead) > 1) {
      stop('Net Information header not properly detected!')
    }
    # grab tow metadata header
    towmetahead <- which(linedata == 'Tow Configuration Information')
    if (length(towmetahead) == 0 | length(towmetahead) > 1) {
      stop('Tow Metadata header not properly detected!')
    }

    # gather lines into sections
    netinfodata <- linedata[(netinfohead+1):(towmetahead-1)]
    towmetadata <- linedata[(towmetahead+1):length(linedata)]

    # tidy each section
    sectionsplitind <- grep(netinfodata, pattern = '===')
    netinfodata <- netinfodata[-sectionsplitind]
    emptylineind <- which(netinfodata == "")
    if (length(emptylineind) > 0) {
      netinfodata <- netinfodata[-emptylineind]
    }

    sectionsplitind <- grep(towmetadata, pattern = '===')
    towmetadata <- towmetadata[-sectionsplitind]
    emptylineind <- which(towmetadata == "")
    if (length(emptylineind) > 0) {
      towmetadata <- towmetadata[-emptylineind]
    }

    # format into bioness object

    data <- list('metadata'= list(), 'data' = data.frame())
    class(data) <- 'BIONESS'

    towmetadata_names <- str_trim(str_split_i(towmetadata, pattern = '=', i = 1), side = 'both')
    towmetadata_values <- str_trim(str_split_i(towmetadata, pattern = '=', i = 2), side = 'both')

    data$metadata <- as.list(towmetadata_values)
    names(data$metadata) <- towmetadata_names

    netdataframe <- str_split(netinfodata, pattern = ",")
    netdataframe <- as.data.frame(netdataframe,
                                  row.names = c('net_number', 'time', 'depth', 'volume', 'temperature', 'salinity' ,'strobe'))
    names(netdataframe) <- NA

    data$data <- data.frame(t(netdataframe))

    return(data)
    }


}
