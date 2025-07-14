### {r manual-pm-equal}
## correspondence/concordance table
codes_BA <- dplyr::tribble(
    ~std_B, ~std_A,
    "A1", "x1111", # one-to-one
    "B2", "x2222", # many-to-one
    "B2", "x3333",
    "C3", "x4444", # one-to-many (4)
    "C4", "x4444",
    "C4", "x6666", # many-to-many
    "C5", "x4444",
    "C6", "x4444",
    "C7", "x5555", # one-to-many (3)
    "C8", "x5555",
)

## panel_map
weights_BA <- codes_BA |>
    dplyr::distinct(std_B, std_A) |>
    dplyr::group_by(std_A) |>
    dplyr::mutate(
        n_dest = dplyr::n(),
        weight = 1 / n_dest
    ) |>
    dplyr::ungroup()

pm_BA <- weights_BA |>
    dplyr::select(std_B, std_A, weight) |>
    dplyr::arrange(std_A)

set.seed(1832)
std_A_codes <- sort(unique(codes_BA$std_A))
data_A <-
    dplyr::tibble(
        std_A = std_A_codes,
        A_100 = 100,
        A_prod = round(abs(rnorm(length(std_A_codes)) * 10000), 0)
    )

error_highlight_col <- "#d88080"

## bad data example

data_missing <- data_A |>
  dplyr::arrange(std_A) |>
  dplyr::select(std_A, A_prod)
data_missing[data_missing$std_A == "x5555", c("A_prod")] <- NA
data_missing <- data_missing |>
  dplyr::select(`Industry` = "std_A", `GDP` = "A_prod")

x_bad <- data_A |>
  dplyr::add_row(
    std_A = "x7777",
    A_100 = 100,
    A_prod = 3895
  ) |> dplyr::arrange(std_A)
  
x_bad_df <- x_bad |>
    dplyr::select(`Industry` = "std_A", `GDP` = "A_prod")

x_bad2_df <- x_bad_df

x_bad2_df[x_bad2_df$Industry == "x5555", c("GDP")] <- NA


