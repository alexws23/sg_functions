process_port <- function(dat,
                         port_name,
                         sequence_to_ID,
                         signal_duration,
                         code,
                         output_dir) {

  cat("Processing", port_name, "\n")

  if(nrow(dat) < 4) return(NULL)

  ###################### Get time difference ######################

  df <- dat |>
    dplyr::mutate(
      diff = time - dplyr::lag(time, default = dplyr::first(time))
    )

  ################ Remove very short intervals ###################

  df2 <- subset(df, diff > signal_duration)

  if(nrow(df2) < 4) return(NULL)

  df2$diff <- round(df2$diff, 3)

  ################ Search for pulse code #########################

  sequence_found <- sapply(
    seq_len(nrow(df2)),
    function(i) {

      any(
        sapply(
          sequence_to_ID,
          function(seq_test) {

            end_index <- i + length(seq_test) - 1

            if(end_index > nrow(df2))
              return(FALSE)

            identical(
              df2$diff[i:end_index],
              seq_test
            )
          }
        )
      )
    }
  )

  df2$targets <- as.integer(sequence_found)

  ################ Mark pulse sequence ###########################

  df2$targets_2 <- change_next_three_cells(df2$targets)

  df2 <- subset(df2, targets_2 != 0)

  if(nrow(df2) < 4) return(NULL)

  rownames(df2) <- NULL

  ################ Group into 4 pulse sequence ###################

  df2$group <- rep_len(1:4, nrow(df2))

  dd <- df2 |>
    dplyr::group_by(group) |>
    dplyr::mutate(n = dplyr::row_number()) |>
    tidyr::spread(group, diff) |>
    dplyr::select(-n)

  dd[is.na(dd)] <- 0

  dd$targets <- NULL
  dd$targets_2 <- NULL

  ###############################################################

  pnum <- gsub("p", "", port_name)

  colnames(dd) <- c(
    "port",
    "time",
    paste0("freq_", pnum),
    paste0("power_", pnum),
    paste0("noise_", pnum),
    paste0("S2N_", pnum),
    paste0("P1_", pnum),
    paste0("P2_", pnum),
    paste0("P3_", pnum),
    paste0("PInt_", pnum)
  )

  ###############################################################

  port_out <- dd |>
    dplyr::group_by(grp = as.integer(gl(dplyr::n(), 4, dplyr::n()))) |>
    dplyr::summarise(
      time = min(time),

      freq = mean(.data[[paste0("freq_", pnum)]], na.rm = TRUE),

      power = max(.data[[paste0("power_", pnum)]], na.rm = TRUE),

      noise = max(.data[[paste0("noise_", pnum)]], na.rm = TRUE),

      S2N = mean(.data[[paste0("S2N_", pnum)]], na.rm = TRUE),

      PI_1 = max(.data[[paste0("P1_", pnum)]], na.rm = TRUE),

      PI_2 = max(.data[[paste0("P2_", pnum)]], na.rm = TRUE),

      PI_3 = max(.data[[paste0("P3_", pnum)]], na.rm = TRUE),

      PInt = max(.data[[paste0("PInt_", pnum)]], na.rm = TRUE)
    )

  port_out$grp <- NULL

  port_out$time <- as.POSIXct(
    port_out$time,
    origin = "1970-01-01",
    tz = "UTC"
  )

  ################ Write outputs ################################

  return(port_out)
}
