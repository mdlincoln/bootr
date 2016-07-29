#' Bootstrap calculations on a data frame
#'
#' @param df A data frame
#' @param m Number of replications (Defaults to 100)
#' @param group_vars A character vector of variable names by which to group
#' @param source_var A single column name on which to calculate the summary
#'   statistic.
#' @param boot_fun A summary function (defaults to \link{\code{mean}}) that will
#'   be used to calculate the boostrapped statistic
#' @param ... Additional arugments passed to \code{boot_fun}
#'
#' @return A summary data frame with the columns \code{group_vars} and
#'   \code{boot_low} (2.5th percentile), \code{boot_med} (50th percentile), and
#'   \code{boot_high} (97.5th percentile).
#'
#' @import dplyr
#'
#' @export
bootr <- function(df, m = 100, group_vars, source_var, boot_fun = mean, ...) {

  internal_boot_fun <- function(dff, gv, sv, cf) {
    summary_dots <- setNames(list(lazyeval::interp(~ cf(var, ...), var = as.name(sv))), "calculated")

    dff %>%
      group_by_(.dots = gv) %>%
      summarize_(.dots = summary_dots)
  }

  df %>%
    broom::bootstrap(df = ., m = m) %>%
    do(internal_boot_fun(., group_vars, source_var, boot_fun, ...)) %>%
    group_by_(.dots = group_vars) %>%
    summarize(
      boot_low = quantile(calculated, 0.025),
      boot_med = quantile(calculated, 0.5),
      boot_high = quantile(calculated, 0.975))
}
