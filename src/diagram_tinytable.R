source(here::here("./src/_setup-pm_BA.R"))

library(tinytable)
tinytable_uncovered_x <- tt(x_bad_df, theme = "grid") |>
  style_tt(i = 7, background = error_highlight_col)

tinytable_missing_x <- tt(data_missing, theme = "grid") |>
  style_tt(i = 5, background = error_highlight_col)

tinytable_both <- tt(x_bad2_df, theme = "grid") |>
  style_tt(i = 7, background = error_highlight_col) |>
  style_tt(i = 5, background = error_highlight_col)
