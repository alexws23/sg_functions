change_next_three_cells <- function(column) {

  if(length(column) < 4) return(column)

  for (i in 1:(length(column)-3)) {

    if (column[i] == 1) {

      column[i+1] <- 2
      column[i+2] <- 3
      column[i+3] <- 4

    }
  }

  column
}
