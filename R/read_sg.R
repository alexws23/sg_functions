#' Read and Clean Compiled Files
#'
#' This function reads and cleans the compiled .txt file produced by the [extract_sg()] function.
#' @param x the file name and path of the .txt file you want to read.
#' @param keep_only_ports logical. Specifies if you want to filter out all data except pulse data. Defaults to TRUE. It is highly recommended that you do not change this parameter unless you have experience working with this data.

read_sg <- function(x,
                    keep_only_ports = TRUE) {

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
    data <- data[grepl("^p", data$V1), ]
  }

  colnames(data) <- c(
    "port",
    "time",
    "freq",
    "power",
    "noise"
  )

  # Convert types
  data$time <- as.numeric(data$time)
  data$power <- as.numeric(data$power)
  data$noise <- as.numeric(data$noise)

  # Signal-to-noise ratio
  data$S2N <- abs(data$noise - data$power)

  return(data)
}

