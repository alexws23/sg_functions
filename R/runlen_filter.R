#' Create a Run Length Filter
#'
#' This function adds a run length filter comparable to the "motusFilter" to a dataframe produced by [search_code()].
#' @param x a data frame created by the [search_code()] function.
#' @param burst_interval the tag's burst interval (eg. 6.7 or 15.1).
#' @param gap the number of bursts that can be missed before a new run starts. Defaults to 13.

runlen_filter <- function(x, burst_interval,
                          gap = 13) {


run_gap <- gap*burst_interval

df <- x |>
  dplyr::group_by(port) |>
  dplyr::arrange(time) |>
  dplyr::mutate(
    diff = as.numeric(difftime(time, dplyr::lag(time), units = "secs")),
    new_run = is.na(diff) | diff >= run_gap,
    runID = cumsum(new_run)
  ) |>
  dplyr::mutate(runID = paste(port,runID,sep = "_")) |>
  dplyr::ungroup()

data_filtered <- df |>
  dplyr::group_by(runID) |>
  dplyr::summarise(runLen = dplyr::n()) |>
  dplyr::mutate(runlen_filter = ifelse(runLen <= 3, 0, 1)) |>
  dplyr::ungroup()

tmp <-  df |>
  dplyr::left_join(data_filtered, by = dplyr::join_by(runID)) |>
  dplyr::select(-c(new_run, diff))

return(tmp)

}
