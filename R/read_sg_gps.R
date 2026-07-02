#' Read in Sensorgnome GPS Data
#'
#' This function reads the complied .txt file created by the [extract_sg()] function and outputs a data fram with just the Sensorgnome GPS data, which is useful for diagnosing issues.
#' @param x the file name and path of the .txt file you want to read.
#' @param keep_only_ports logical. Specifies if you want to filter out all data except pulse data. Defaults to TRUE. It is highly recommended that you do not change this parameter unless you have experience working with this data.
#' @export

read_sg_gps <- function(x, tz = "UTC") {

  if (endsWith(x,".txt") == FALSE) {
    stop("Please specify a .txt file")
  }

  # Read data
  data <- utils::read.csv(
    x,
    header = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE
  )

  if (keep_only_ports == TRUE) {
    # Keep only ports
    data <- data[grepl("^G", data$V1), ]
  }

  colnames(data) <- c(
    "GPS",
    "time",
    "lat",
    "lon",
    "alt"
  )

  # Convert types
  data$time <- as.numeric(data$time)
  data$time <- as.POSIXct(data$time, origin="1970-01-01", tz = "UTC")
  data$time <-  lubridate::with_tz(data$time, tzone = tz)

  return(data)
}
