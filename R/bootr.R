#' Bootstrap calculations on a data frame
#'
#' @param df A data frame
#' @param m Number of replications
#' @param group_vars A character vector of variable names by which to group
#' @param boot_fun A function that accepts
#'
#' @return A summary data frame with the columns \code{group_vars} and \code{boot_low} (2.5th percentile), \code{boot_med} (50th percentile), and \code{boot_high} (97.5th percentile).
#'
#' @import dplyr
#'
#' @export
bootr <- function(df, m = 100, group_vars, boot_fun, ...) {
  df %>%
    broom::bootstrap(m = m) %>%
    do(boot_fun(., group_vars, source_var, ...)) %>%
    group_by_(group_vars) %>%
    summarize(
      boot_low = quantile(calculated, 0.025),
      boot_med = quantile(calculated, 0.5),
      boot_high = quantile(calculated, 0.975))
}

boot_mean <- function(df, group_vars, source_var, ...) {
  dots <- list(calculated = ~mean(source_var))

  df %>%
    group_by_(.dots = group_vars) %>%
    summarize_(.dots = dots)
}
