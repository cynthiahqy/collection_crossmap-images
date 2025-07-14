### ```{r doc-fnc-plt-pm-sigmoid}
library(ggbump)
library(cowplot) ## for theme only
library(dplyr)
library(ggplot2)

# Plot an incidence table (expanded panel map) as a ggplot sigmoid plot
plt_pm_sigmoid <- function(
    pm, from, to, weights,
    .theme = list(
      cowplot::theme_minimal_grid(font_size = 11, line_size = 0),
      theme(
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(2, 0, 0, 2)
      )
    )) {
  edges <- pm |>
    transmute(from = {{ from }}, to = {{ to }}, weighted = {{ weights }})

  ## calculate positions for nodes
  from_nodes <- distinct(edges, from) |> mutate(from_y = row_number())
  to_nodes <- distinct(edges, to) |> mutate(to_y = row_number() - 1 + 0.5)

  ## generate df for ggplot
  df <- edges |>
    ## generate mapping type/case variables
    group_by(from) |>
    mutate(n_dest = n()) |>
    ungroup() |>
    group_by(to) |>
    mutate(
      n_origin = n(),
      min_weight = min(weighted)
    ) |>
    ungroup() |>
    mutate(value_case = case_when(
      n_dest == 1 ~ "one-to-one",
      n_dest > 1 ~ "one-to-many"
    )) |>
    left_join(
      tribble(
        ~value_case, ~line_type, ~font_type,
        "one-to-one", "solid", "bold",
        "one-to-many", "dashed", "italic"
      ),
      by = "value_case"
    ) |>
    mutate(
      from_case = case_when(
        n_origin == 1 ~ "one-from-one",
        n_origin > 1 ~ "one-from-many",
        n_origin < 1 ~ "ERROR! origin codes < 1"
      ),
      dest_case = case_when(
        min_weight < 1 ~ "contains split",
        min_weight == 1 ~ "aggregation only",
        min_weight > 1 ~ "ERROR! weight > 1"
      )
    ) |>
    ## add y-coordinates
    left_join(from_nodes, by = "from") |>
    left_join(to_nodes, by = "to") |>
    ## add x-coordinates
    mutate(
      from_x = 0,
      to_x = 5
    ) |>
    ## give each from-out instruction a unique id
    mutate(idx = row_number())

  plt_uw <- df |>
    ggplot(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, group = idx)) +
    ## edges as sigmoid curves with line type
    geom_sigmoid(aes(linetype = I(line_type))) +
    # to/from nodes
    scale_y_reverse() +
    geom_text(aes(x = from_x - 0.5, label = from, fontface = I(font_type))) +
    geom_label(aes(x = to_x + 0.5, y = to_y, label = to, fill = dest_case)) +
    # edge labels
    geom_label(
      data = filter(df, value_case == "one-to-many"),
      aes(
        x = (((from_x + to_x) / 2) + to_x) / 2,
        y = to_y,
        label = weighted
      )
    ) +
    geom_label(
      data = filter(df, value_case == "one-to-one"),
      aes(
        x = (from_x + to_x) / 4,
        y = from_y,
        label = weighted
      )
    ) +
    labs(x = NULL, y = NULL, fill = "target-from-sources") +
    .theme

  return(plt_uw)
}

##

source(here::here("./src/_setup-pm_BA.R"))

gg_pm_BA <- pm_BA |>
  plt_pm_sigmoid(from = std_A, to = std_B, weights = weight) +
  scale_fill_brewer(palette = "PuBu", direction = -1)

data_missing <- data_A |>
  dplyr::arrange(std_A) |>
  dplyr::select(std_A, A_prod)
data_missing[data_missing$std_A == "x5555", c("A_prod")] <- NA
data_missing <- data_missing |>
  dplyr::select(`Industry` = "std_A", `GDP` = "A_prod")

gt_missing <- gt::gt(data_missing) |>
  gtExtras::gt_highlight_rows(
    rows = is.na(Industry), fill = error_highlight_col
  )

## gt::gtsave(gt_missing, "src/output/diagram_missing-val-input-array-gt.png")

library(tinytable)
tinytable_missing_x <- tt(data_missing, theme = "grid") |>
  style_tt(i = 5, background = error_highlight_col)


library(patchwork)

# wrap_table(gt_missing, space = "fixed_y", panel = "full") +
bigraph <- gg_pm_BA + guides(fill = "none") + ggtitle("")
ggsave("src/output/diagram_missing-val-bigraph-only.png", plot = bigraph, width = 5, height = 3.5)

input_array <- wrap_table(gt_missing, panel = "full")
ggsave("src/output/diagram_missing-val-input-array.png", plot = input_array, width = 1.5, height = 3.5)
