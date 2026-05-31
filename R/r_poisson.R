#' Generate random Poisson distributed numbers
#'
#' This function generates a user-specified number of random numbers
#' from a Poisson distribution.
#'
#' @param n Number of random numbers to generate. Must be a positive integer.
#' @param lambda The mean (and variance) of the Poisson distribution. Must be positive.
#'
#' @return A numeric vector of n random numbers from Poisson(lambda)
#' @export
#'
#' @examples
#' # Generate 100 random numbers from Poisson(mean=5)
#' r_poisson(n = 100, lambda = 5)
#'
#' # Generate 50 random numbers from Poisson(mean=2.5)
#' r_poisson(n = 50, lambda = 2.5)
r_poisson <- function(n, lambda) {
  # Input validation
  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("n must be a positive integer")
  }
  if (!is.numeric(lambda) || lambda <= 0) {
    stop("lambda must be a positive number")
  }

  # Generate Poisson random numbers
  set.seed(123)  # for reproducibility (optional)
  numbers <- rpois(n = n, lambda = lambda)

  return(numbers)
}
