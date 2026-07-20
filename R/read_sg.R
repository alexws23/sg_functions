#' Read and Clean Compiled Files
#'
#' This function reads and cleans the compiled .txt file produced by the [extract_sg()] function.
#' @param x the file name and path of the .txt file you want to read.
#' @param keep_only_ports logical. Specifies if you want to filter out all data except pulse data. Defaults to TRUE. It is highly recommended that you do not change this parameter unless you have experience working with this data.
#' @export

read_sg <- function(x,
                    keep_only_ports = TRUE) {

   if (endsWith(x,".txt") == FALSE) {
     stop("Please specify a .txt file")
  }

  lines <- readLines(x)

  # Split each line by comma
  split_lines <- strsplit(lines, ",")

  # Force each row to have exactly 6 columns
  split_fixed <- lapply(split_lines, function(x) {
    length(x) <- 6   # pad with NAs or truncate to length 6
    x
  })

  # Convert to data frame
  data <- as.data.frame(do.call(rbind, split_fixed), stringsAsFactors = FALSE)

  # Assign column names
  colnames(data) <- c("port","time","freq","power","noise","S2N")

  if (keep_only_ports == TRUE) {
    # Keep only ports
    # Remove rows starting with C, G, or S
    data <- data[!substr(data$port, 1, 1) %in% c("C", "G", "S"), ]
  }

  # Convert types
  data$time <- as.numeric(data$time)
  data$power <- as.numeric(data$power)
  data$noise <- as.numeric(data$noise)

  return(data)
}

