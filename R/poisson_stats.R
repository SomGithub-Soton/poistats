#' Calculate summary statistics for Poisson numbers
#'
#' This function takes a numeric vector and returns comprehensive summary statistics.
#'
#' @param x A numeric vector (typically from r_poisson function)
#'
#' @return A list containing various summary statistics
#' @export
#'
#' @examples
#' # Generate some numbers
#' set.seed(123)
#' numbers <- r_poisson(n = 100, lambda = 5)
#'
#' # Calculate statistics
#' poisson_stats(numbers)
poisson_stats <- function(x) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  if (length(x) == 0) {
    stop("Vector is empty")
  }

  # Calculate statistics
  stats_list <- list(
    sample_size = length(x),
    min = min(x),
    max = max(x),
    range = max(x) - min(x),
    mean = mean(x),
    median = median(x),
    variance = var(x),
    standard_deviation = sd(x),
    first_quartile = quantile(x, 0.25),
    third_quartile = quantile(x, 0.75),
    interquartile_range = IQR(x)
  )

  # Print nicely formatted results
  cat("\n========== Poisson Distribution Summary Statistics ==========\n")
  cat(paste("Sample Size:", stats_list$sample_size, "\n"))
  cat(paste("Minimum:", round(stats_list$min, 4), "\n"))
  cat(paste("Maximum:", round(stats_list$max, 4), "\n"))
  cat(paste("Range:", round(stats_list$range, 4), "\n"))
  cat(paste("Mean:", round(stats_list$mean, 4), "\n"))
  cat(paste("Median:", round(stats_list$median, 4), "\n"))
  cat(paste("Variance:", round(stats_list$variance, 4), "\n"))
  cat(paste("Standard Deviation:", round(stats_list$standard_deviation, 4), "\n"))
  cat(paste("First Quartile (Q1):", round(stats_list$first_quartile, 4), "\n"))
  cat(paste("Third Quartile (Q3):", round(stats_list$third_quartile, 4), "\n"))
  cat(paste("Interquartile Range (IQR):", round(stats_list$interquartile_range, 4), "\n"))
  cat("=============================================================\n\n")

  # Return the list invisibly so it doesn't print twice
  invisible(stats_list)
}
