library(ggplot2)

## ```{r doc-fnc-plt-data-mtx}
plt_df_mtx <- function(x, cols_from, row_names) {
  x |>
    dplyr::select({{ row_names }}, {{ cols_from }}) |>
    tidyr::pivot_longer({{ cols_from }},
      names_to = "var", values_to = "value"
    ) |>
    ggplot(aes(x = var, y = {{ row_names }})) +
    geom_tile(aes(fill = var), col = "grey") +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top") +
    scale_fill_brewer(palette = "Greens") +
    coord_fixed() +
    labs(x = element_blank(), y = element_blank()) +
    theme_minimal() +
    theme(legend.position = "none")
}

### ```{r doc-fnc-inc-long-plts}
plt_inc_long_mtx <- function(inc_long, to, from, weights) {
  gg <- inc_long |>
    dplyr::mutate(src_case = dplyr::case_when(
      {{ weights }} == 1 ~ "one-to-one",
      is.na({{ weights }}) ~ "none",
      {{ weights }} < 1 ~ "one-to-many"
    )) |>
    ggplot(aes(x = {{ to }}, y = {{ from }})) +
    geom_tile(aes(fill = src_case), col = "grey", show.legend = FALSE) +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top") +
    scale_fill_brewer() +
    coord_fixed() +
    labs(x = element_blank(), y = element_blank(), fill = "source-to-target") +
    theme_minimal()
  return(gg)
}

## conformr-project/create-conformr/02-basics.Rmd

source(here::here("./src/_setup-pm_BA.R"))

### ```{r viz-matrix-pm-equal}
library(ggplot2)

inc_long <- tidyr::expand(codes_BA, std_A, std_B) |>
  dplyr::left_join(pm_BA, by = c("std_A", "std_B")) |>
  dplyr::transmute(to = std_B, from = std_A, weight = weight)

gg_inc_mtx <- inc_long |>
  plt_inc_long_mtx(to, from, weight) +
  ggtitle("Concordance as Incidence Matrix")

gg_pm_mtx <- gg_inc_mtx +
  geom_text(data = dplyr::filter(inc_long, !is.na(weight)), aes(label = round(weight, 2))) +
  ggtitle("adding equal weights for Valid Panel Map")


## ```{r gg-data-in-mtx}
gg_x_mtx <- plt_df_mtx(data_A, A_100:A_prod, std_A)

library(patchwork)
gg_pm_mtx +
  guides(fill = "none") +
  ggtitle("") +
  gg_x_mtx +
  scale_y_discrete(position = "right", limits = rev) +
  patchwork::plot_annotation(title = "Crossmap covers Source Data")

## ```{r gg-no-coverage}
x_bad <- data_A |>
  dplyr::add_row(
    std_A = "x7777",
    A_100 = 100,
    A_prod = 3895
    |> dplyr::arrange(std_A)
  )

gg_x_bad <- x_bad |>
  plt_df_mtx(A_prod, std_A)

x_bad_df <- x_bad |>
  dplyr::select(`Industry` = "std_A", `GDP` = "A_prod")

gt_x_bad <- x_bad
gt::gt() |>
  gtExtras::gt_highlight_rows(
    rows = `Industry` == "x7777",
    fill = error_highlight_col
  )

library(tinytable)
tinytable_uncovered_x <- tt(x_bad_df, theme = "grid") |>
  style_tt(i = 7, background = error_highlight_col)

library(patchwork)
gg_pm_mtx +
  # guides(fill = "none") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ggtitle("") +
  wrap_table(gt_x_bad, panel = "full", space = "free_x")
ggsave("src/output/diagram_no-coverage-default.png", plot = last_plot())
# scale_y_discrete(position = "left", limits = rev) +
# scale_fill_brewer(palette = "Purples") +
# patchwork::plot_annotation(title = "Crossmap does not cover fully source index (I)")

## -- with NA ---
gg_pm_bad <- tidyr::expand_grid(from = c(NA), to = unique(codes_BA$std_B)) |>
  dplyr::mutate(weight = NA) |>
  dplyr::bind_rows(inc_long) |>
  plt_inc_long_mtx(to, from, weight) +
  geom_text(data = dplyr::filter(inc_long, !is.na(weight)), aes(label = round(weight, 2)))
