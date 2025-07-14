theme_minimal_tilematrix <- function(base_size = 11,
                                     base_family = "",
                                     base_line_size = base_size / 22,
                                     base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_blank(),
      complete = TRUE
    )
}



## ```{r doc-fnc-plt-data-mtx}
plot_df_as_tilematrix <- function(
    df, cols_from, row_names,
    .geom = list(
      geom_tile(aes(fill = cols_names), col = "grey"),
      geom_text(aes(label = cols_value, size = 3))
    ),
    .scale_coord = list(
      scale_y_discrete(limits = rev),
      scale_x_discrete(position = "top"),
      scale_fill_brewer(palette = "Greens"),
      coord_fixed()
    ),
    .labs = list(labs(x = element_blank(), y = element_blank())),
    .theme = list(
      theme_minimal(),
      theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
    )) {
  df |>
    dplyr::select({{ row_names }}, {{ cols_from }}) |>
    tidyr::pivot_longer({{ cols_from }},
      names_to = "cols_names", values_to = "cols_value"
    ) |>
    ggplot(aes(x = cols_names, y = {{ row_names }})) +
    .geom +
    .scale_coord +
    .labs +
    .theme
}

## testing
df_for_tilematrix <- data.frame(
  x = LETTERS[1:7],
  col_num1 = sample(seq(1, 100), 7),
  col_text1 = sample(letters, 7),
  col_num2 = sample(seq(1, 100), 7),
  col_text2 = sample(letters, 7)
)

plot_df_as_tilematrix(df_for_tilematrix,
  cols_from = dplyr::starts_with("col_num"),
  row_names = x
)

plot_df_as_tilematrix(df_for_tilematrix,
  cols_from = dplyr::starts_with("col_num"),
  row_names = x,
  .theme = list(theme_minimal_tilematrix())
)

## TODO: catch error and tell people you can't plot different data-types in a matrix!
plot_df_as_tilematrix(df_for_tilematrix,
  cols_from = dplyr::starts_with("col_"),
  row_names = x
)

## ---



### ```{r doc-fnc-inc-long-plts}
add_weight_case <- function(links, to, from, weights) {

}

plot_edgelist_as_tilematrix <- function(
    df, col_names, row_names, cell_fill, cell_values,
    .geom = list(
      geom_tile(aes(fill = cell_fill), col = "grey", show.legend = FALSE),
      geom_text(aes(labels = cell_values))
    ),
    .scale_coord = list(
      scale_y_discrete(limits = rev),
      scale_x_discrete(position = "top"),
      # scale_fill_brewer(),
      coord_fixed()
    ),
    .labs = list(
      labs(
        x = element_blank(),
        y = element_blank(),
        fill = element_blank()
      )
    ),
    .theme = list(
      theme_minimal(),
      theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
    )) {
  df |>
    ggplot(aes(
      x = {{ col_names }}, y = {{ row_names }},
      fill = {{ cell_values }}, label = {{ cell_values }}
    )) +
    .geom +
    .scale_coord +
    .labs +
    .theme
}

inc_long |>
  plot_edgelist_as_tilematrix(to, from, "blue", weight)
