#' Search Sensorgnome Data for a Known Codeset
#'
#' This function searches through Sensorgnome data processed with the [read_sg())] to find the specific pulse code set associated with a tag.
#' @param x a data frame created by the [read_sg()] function.
#' @param code the tag's 3-number code.
#' @param pulse_1 the first pulse gap in milliseconds.
#' @param pulse_2 the second pulse gap in milliseconds.
#' @param pulse_3 the third pulse gap in milliseconds.
#' @param upper_freq the upper frequency offset threshold. Defaults to 5.
#' @param lower_freq the upper frequency offset threshold. Defaults to 3.
#' @param signal_duration the minimum signal duration cutoff. Defaults to 0.015 milliseconds.
#' @param S2N_cutoff the minimum signal to noise ratio allowed. Defaults to 8.
#' @param tz specifies which timezone you want the data to output in. A character string. The time zone specification to be used for the conversion, if one is required. System-specific ([`time zones`]), but "" is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning. Defaults to "UTC"
#' @param create_csv logical. If TRUE, output_file and output_dir must be specified. Defaults to FALSE.
#' @param output_file the name of the .csv file to be created.
#' @param output_dir the directory you want the file to be stored in. If no directory is specified, the file will be created in the working directory.
#' @export
#' @details
#' This process is not meant to replace the process used by the broader Motus network, but is simply meant to be used to quickly process raw Sensorgnome data to diagnose deployment issues or search known tags quickly.


search_code <- function(x,
                        code,
                        pulse_1,
                        pulse_2,
                        pulse_3,
                        upper_freq = 5,
                        lower_freq = 3,
                        signal_duration = 0.015,
                        S2N_cutoff = 8,
                        tz = "UTC",
                        create_csv = FALSE,
                        output_file = NULL,
                        output_dir = NULL
                        ) {

  if (ncol(x) != 6) {
    stop("Six columns not detected. Double check that you are using the correct data and ran it through read_sg()")
  }

  '%ni%' <- Negate('%in%')

  if (any(colnames(x) %ni% c("port", "time", "freq", "power", "noise", "S2N"))) {
    stop("Column names not compatible. Ensure data was read using read_sg()")
  }

  if (endsWith(output_dir,"/") == TRUE) {
    stop("Please do not include "/" at the end of your output directory")
  }

  # Filters
  data <- subset(x, S2N > S2N_cutoff)
  data <- subset(data,
                 freq > lower_freq &
                   freq < upper_freq)

  # Split by port
  port_data <- split(data, data$port)

  # Generate pulse combinations
  sequence <- c(pulse_1, pulse_2, pulse_3)

  sequence_to_ID <- generate_combinations(sequence)

  # Process each port
  results <- lapply(
    names(port_data),
    function(port_name) {

      process_port(
        dat = port_data[[port_name]],
        port_name = port_name,
        sequence_to_ID = sequence_to_ID,
        signal_duration = signal_duration,
        code = code,
        output_dir = output_dir
      )

    }
  )


  names(results) <- names(port_data)

  # Remove NULL ports
  results <- Filter(Negate(is.null), results)

  all_ports <- dplyr::bind_rows(results, .id = "port")

  all_ports$port <- gsub("p", "", all_ports$port)
  sg_data <- all_ports[order(all_ports$time), ]
  sg_data$time <- as.POSIXct(sg_data$time, origin="1970-01-01", tz = "UTC")
  sg_data <- sg_data |>
    dplyr::mutate_if(is.character,as.numeric) |>
    tidyr::drop_na(time)

  tag_sg <- sg_data|>
    dplyr::mutate(port = as.character(port))|>
    tidyr::drop_na(freq)|>
    dplyr::mutate(time = lubridate::with_tz(time, tzone = tz))

  if (create_csv == TRUE) {
    dir.create(output_dir, showWarnings = FALSE)
    utils::write.csv(tag_sg, file = paste0(output_dir,"/",output_file))
  }

  return(tag_sg)
}
