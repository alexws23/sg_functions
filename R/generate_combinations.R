generate_combinations <- function(sequence) {

  combinations <- list()

  for (j in c(-0.001, 0, 0.001)) {
    for (k in c(-0.001, 0, 0.001)) {
      for (l in c(-0.001, 0, 0.001)) {

        combination <- sequence + c(j, k, l)

        combinations <- c(combinations,
                          list(round(combination, 3)))
      }
    }
  }

  combinations
}
