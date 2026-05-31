#' Create comprehensive plots for Poisson numbers
#'
#' This function creates a grid of plots including histogram, boxplot,
#' violin plot, and density distribution plot.
#'
#' @param x A numeric vector (typically from r_poisson function)
#' @param title Optional title for the plots
#'
#' @return A combined grid of plots (invisibly)
#' @export
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' # Generate some numbers
#' set.seed(123)
#' numbers <- r_poisson(n = 100, lambda = 5)
#'
#' # Create plots
#' plot_poisson(numbers)
#'
#' # Add a custom title
#' plot_poisson(numbers, title = "My Poisson Distribution Analysis")
plot_poisson <- function(x, title = NULL) {
  # Check if required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed. Please install it with: install.packages('ggplot2')")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is needed. Please install it with: install.packages('patchwork')")
  }

  # Load packages
  library(ggplot2)
  library(patchwork)

  # Input validation
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  if (length(x) == 0) {
    stop("Vector is empty")
  }

  # Create data frame for ggplot
  df <- data.frame(value = x)

  # 1. Histogram
  p1 <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = max(5, diff(range(x))),
                   fill = "skyblue",
                   color = "black",
                   alpha = 0.7) +
    labs(title = "Histogram",
         x = "Value",
         y = "Density") +
    theme_minimal()

  # 2. Boxplot
  p2 <- ggplot(df, aes(y = value)) +
    geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = "Boxplot",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())

  # 3. Violin plot
  p3 <- ggplot(df, aes(x = factor(1), y = value)) +
    geom_violin(fill = "lightcoral", color = "black", alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.5) +
    labs(title = "Violin Plot",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())

  # 4. Density plot
  p4 <- ggplot(df, aes(x = value)) +
    geom_density(fill = "lightyellow", color = "black", alpha = 0.7) +
    labs(title = "Density Distribution",
         x = "Value",
         y = "Density") +
    theme_minimal()

  # Combine plots using patchwork
  combined_plot <- (p1 + p2) / (p3 + p4)

  # Add main title if provided
  if (!is.null(title)) {
    combined_plot <- combined_plot +
      plot_annotation(title = title,
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  } else {
    combined_plot <- combined_plot +
      plot_annotation(title = paste("Poisson Distribution Analysis (n=", length(x), ")", sep = ""),
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  }

  # Display the plot
  print(combined_plot)

  # Return invisibly
  invisible(combined_plot)
}
