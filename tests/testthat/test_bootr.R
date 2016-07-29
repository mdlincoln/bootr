context("Bootr integration")
suppressPackageStartupMessages(library(dplyr))

test_that("Bootstrap calculated correctly", {

  m <- 10

  set.seed(100)
  man_boot_single <- mtcars %>%
    broom::bootstrap(df = ., m = m) %>%
    do(summarize(group_by(., gear), calculated = mean(mpg))) %>%
    group_by(gear) %>%
    summarize(
      boot_low = quantile(calculated, 0.025),
      boot_med = quantile(calculated, 0.5),
      boot_high = quantile(calculated, 0.975))

  set.seed(100)
  pkg_boot_single <- mtcars %>%
    bootr(m = m, group_vars = "gear", source_var = "mpg", boot_fun = boot_mean)

  expect_equivalent(man_boot_single, pkg_boot_single)

  set.seed(100)
  man_boot_multi <- mtcars %>%
    broom::bootstrap(df = ., m = m) %>%
    do(summarize(group_by(., gear, carb), calculated = mean(mpg))) %>%
    group_by(gear, carb) %>%
    summarize(
      boot_low = quantile(calculated, 0.025),
      boot_med = quantile(calculated, 0.5),
      boot_high = quantile(calculated, 0.975))

  set.seed(100)
  pkg_boot_multi <- mtcars %>%
    bootr(m = m, group_vars = c("gear", "carb"), source_var = "mpg", boot_fun = boot_mean)

  expect_equivalent(man_boot_multi, pkg_boot_multi)
})

